using System.Collections.Concurrent;
using System.Diagnostics;
using System.Reflection;
using Spek.Hosting.Console;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Behavioral coverage for the hosting adapters whose control-command /
/// signal routing was previously untested.
///
/// <para><b>Console host</b> (<see cref="SpekConsoleHost"/>, directly
/// referenced by the test project): the existing
/// <see cref="ConsoleHostTests"/> and <see cref="HostingShutdownTests"/>
/// cover the happy-path exit-code round-trip. This class adds the
/// <i>hard-deadline break</i> (an actor that refuses to stop must not
/// hang the host), the <i>no-reply default</i> path (actor stops without
/// surfacing an Option-D code → host returns <c>defaultExitCode</c>), and
/// the null-argument guards on both <c>RunAsync</c> overloads.</para>
///
/// <para><b>Windows / Systemd / Launchd hosted services</b>: these three
/// adapter assemblies are NOT project-referenced by Spek.Tests (the
/// WindowsService one even targets <c>net10.0-windows</c>), but each
/// declares <c>InternalsVisibleTo("Spek.Tests")</c> and its
/// control-command routing is platform-agnostic — the
/// <c>[SupportedOSPlatform]</c> attributes carry no runtime behavior, and
/// the routing methods only call <see cref="ActorRef.Tell(object, ActorRef)"/>.
/// We load the real production assemblies from their build output via
/// reflection and drive their actual <c>StartAsync</c> / control hooks /
/// <c>StopAsync</c> code, asserting the entry actor receives the mapped
/// message and that null-factory hooks are silent no-ops. This exercises
/// the shipping code, not a reimplementation.</para>
/// </summary>
public class HostingAdapterTests
{
    // ================================================================
    //  Shared message vocabulary + a spy actor base.
    // ================================================================

    public sealed record Shutdown();
    public sealed record Pause();
    public sealed record Continue();
    public sealed record Reload();
    public sealed record Custom(int Code);

    /// <summary>
    /// A spy entry actor. The hosted-service adapters spawn the entry
    /// actor with <c>system.Spawn<TActor>()</c> (no constructor
    /// args), so the spy can't be handed a recorder instance — instead it
    /// records into a per-subclass static queue. Tests within a single
    /// xUnit class run sequentially (one collection per class by default),
    /// and each scenario uses its own spy subclass, so the static state
    /// never interleaves. Each test clears its spy's queue up front.
    /// </summary>
    private abstract class SpyActorBase : ActorBase
    {
        protected abstract ConcurrentQueue<object> Sink { get; }

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Sink.Enqueue(message);
            return Task.CompletedTask;
        }
    }

    private static bool WaitFor(Func<bool> cond, TimeSpan timeout)
    {
        var deadline = DateTime.UtcNow + timeout;
        while (!cond() && DateTime.UtcNow < deadline)
            Thread.Sleep(15);
        return cond();
    }

    private static async Task<bool> WaitForAsync(Func<bool> cond, TimeSpan timeout)
    {
        var deadline = DateTime.UtcNow + timeout;
        while (!cond() && DateTime.UtcNow < deadline)
            await Task.Delay(15);
        return cond();
    }

    // ================================================================
    //  Reflection harness for the three not-project-referenced adapters.
    // ================================================================

    private static string RepoRoot()
    {
        var d = new DirectoryInfo(AppContext.BaseDirectory);
        while (d is not null && !File.Exists(Path.Combine(d.FullName, "src", "Spek.slnx")))
            d = d.Parent;
        return d?.FullName
            ?? throw new InvalidOperationException("Could not locate src/Spek.slnx above the test output directory.");
    }

    // The three adapter assemblies are loaded from their build output, not
    // project-referenced, so their package-only dependencies (e.g.
    // Microsoft.Extensions.Hosting.Systemd, which the systemd adapter's
    // SdNotify references) aren't on the test runtime's probing path and
    // aren't copied into the adapter's bin (they're framework/package refs
    // the SDK normally resolves via deps.json). We resolve them from the
    // NuGet global-packages cache + the adapter bins on demand. This loads
    // the REAL production dependency, not a stub.
    private static int _resolverInstalled;

    private static void EnsureResolver()
    {
        if (Interlocked.Exchange(ref _resolverInstalled, 1) != 0) return;

        AppDomain.CurrentDomain.AssemblyResolve += (_, args) =>
        {
            var simpleName = new AssemblyName(args.Name).Name;
            if (simpleName is null) return null;

            // 1) Adapter bin directories.
            foreach (var (proj, tfm) in new[]
            {
                ("Spek.Hosting.Systemd", "net10.0"),
                ("Spek.Hosting.Launchd", "net10.0"),
                ("Spek.Hosting.WindowsService", "net10.0-windows"),
            })
            {
                var candidate = Path.Combine(RepoRoot(), "src", proj, "bin", "Debug", tfm, simpleName + ".dll");
                if (File.Exists(candidate)) return Assembly.LoadFrom(candidate);
            }

            // 2) NuGet global-packages cache: pick the newest package
            //    version, then the highest net*/netstandard* lib folder.
            var nuget = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                ".nuget", "packages", simpleName.ToLowerInvariant());
            if (Directory.Exists(nuget))
            {
                var hit = Directory.GetDirectories(nuget)
                    .OrderByDescending(v => v, StringComparer.Ordinal)
                    .SelectMany(v =>
                    {
                        var lib = Path.Combine(v, "lib");
                        return Directory.Exists(lib)
                            ? Directory.GetDirectories(lib).OrderByDescending(f => f, StringComparer.Ordinal)
                            : Enumerable.Empty<string>();
                    })
                    .Select(f => Path.Combine(f, simpleName + ".dll"))
                    .FirstOrDefault(File.Exists);
                if (hit is not null) return Assembly.LoadFrom(hit);
            }

            return null;
        };
    }

    private static Assembly LoadAdapter(string project, string tfm)
    {
        EnsureResolver();
        var dll = Path.Combine(RepoRoot(), "src", project, "bin", "Debug", tfm, project + ".dll");
        Assert.True(File.Exists(dll),
            $"Adapter assembly not built: {dll}. Build {project}.csproj first.");
        return Assembly.LoadFrom(dll);
    }

    // ----------------------------------------------------------------
    //  Console host — hard-deadline break.
    // ----------------------------------------------------------------

    /// <summary>An entry actor that NEVER stops and never replies — it
    /// ignores Shutdown entirely. The only thing that can end the host's
    /// wait loop is the hard-deadline grace window.</summary>
    private sealed class StubbornActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    [Fact]
    public async Task ConsoleHost_HardDeadlineBreak_ReturnsDefaultWhenActorNeverStops()
    {
        // RunAsync must NOT hang on an actor that refuses to stop. Once
        // shutdown is requested, the bounded grace window expires and the
        // host returns the default exit code with the actor still alive.
        using var system = new ActorSystem("console-hard-deadline");
        var entry        = system.Spawn<StubbornActor>();

        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromMilliseconds(300),
            defaultExitCode: 5);

        // Let the wait loop spin up, then trigger shutdown via the
        // production OnShutdownRequested path. The actor swallows it.
        Assert.True(await WaitForAsync(() => !entry.IsStopped, TimeSpan.FromSeconds(2)),
            "Entry actor should be alive while RunAsync's loop runs.");
        system.RequestShutdown();

        var sw       = Stopwatch.StartNew();
        var exitCode = await hostTask;
        sw.Stop();

        // Returned via the hard deadline, not via the actor stopping.
        Assert.Equal(5, exitCode);
        Assert.False(entry.IsStopped,
            "The stubborn actor never stops; the host must have broken out on the hard deadline.");
        // Bounded (not hung). Generous ceiling tolerates CPU-saturated
        // parallel runs where the 50ms poll continuation stacks up, while
        // still being unmistakably shorter than any "hung forever" case.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(30),
            $"RunAsync should return via the 300ms hard deadline, but took {sw.Elapsed}.");
    }

    // ----------------------------------------------------------------
    //  Console host — actor stops with NO Option-D reply → default code.
    // ----------------------------------------------------------------

    /// <summary>Entry actor that stops itself on Shutdown but never replies
    /// a typed exit code. The host's internal receiver therefore captures
    /// nothing, so RunAsync must surface <c>defaultExitCode</c> — distinct
    /// from the existing round-trip test where the actor DOES reply 42.</summary>
    private sealed class SilentStopActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Shutdown)
                StopSelf();         // no _currentSender.Tell(code) → no Option-D reply
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task ConsoleHost_ActorStopsWithoutReply_SurfacesDefaultExitCode()
    {
        // Drives the full OnShutdownRequested path: RequestShutdown Tells
        // the entry actor Shutdown with the host's internal receiver as the
        // apparent sender. The actor stops WITHOUT replying a code, so the
        // receiver writes nothing and the host returns defaultExitCode.
        using var system = new ActorSystem("console-silent-stop");
        var entry        = system.Spawn<SilentStopActor>();

        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromSeconds(5),
            defaultExitCode: 13);

        Assert.True(await WaitForAsync(() => !entry.IsStopped, TimeSpan.FromSeconds(2)),
            "Entry actor should be alive while RunAsync's loop runs.");
        system.RequestShutdown();

        var exitCode = await hostTask;

        Assert.True(entry.IsStopped,
            "Entry actor should have stopped after handling Shutdown.");
        Assert.Equal(13, exitCode);   // no Option-D reply → default, NOT the round-tripped 42
    }

    // ----------------------------------------------------------------
    //  Console host — null-argument guards.
    // ----------------------------------------------------------------

    [Fact]
    public async Task ConsoleHost_RunAsync_NullArguments_Throw()
    {
        using var system = new ActorSystem("console-null-guards");
        var entry        = system.Spawn<StubbornActor>();

        // The generic RunAsync<TActor> overload is a NON-async method whose
        // ArgumentNullException.ThrowIfNull(shutdownFactory) runs before it
        // returns a Task — so the ANE escapes synchronously. The `{ _ = … }`
        // block body makes the lambda an Action, binding Assert.Throws to
        // its synchronous overload.
        Assert.Throws<ArgumentNullException>(() =>
            { _ = SpekConsoleHost.RunAsync<StubbornActor>(shutdownFactory: null!); });

        // The bring-your-own-system overload forwards to the PRIVATE async
        // RunAsync, whose ThrowIfNull guards fault the returned Task rather
        // than throwing synchronously — so these are awaited via ThrowsAsync.
        await Assert.ThrowsAsync<ArgumentNullException>(() =>
            SpekConsoleHost.RunAsync(null!, entry, () => new Shutdown()));
        await Assert.ThrowsAsync<ArgumentNullException>(() =>
            SpekConsoleHost.RunAsync(system, null!, () => new Shutdown()));
        await Assert.ThrowsAsync<ArgumentNullException>(() =>
            SpekConsoleHost.RunAsync(system, entry, null!));
    }

    // ================================================================
    //  Windows Service adapter (reflection-loaded real production code).
    // ================================================================

    private sealed class WinSpy : SpyActorBase
    {
        public static readonly ConcurrentQueue<object> Box = new();
        protected override ConcurrentQueue<object> Sink => Box;
    }

    /// <summary>Reflection wrapper over SpekWindowsHostedService<WinSpy>.</summary>
    private sealed class WinHost
    {
        private readonly object _svc;
        private readonly Type _t;

        private WinHost(object svc, Type t) { _svc = svc; _t = t; }

        public static WinHost Create(
            Func<object> shutdown,
            Func<object>? pause = null,
            Func<object>? cont = null,
            Func<string, object>? power = null,
            Func<string, object>? session = null,
            Func<int, object>? custom = null,
            TimeSpan? grace = null)
        {
            var asm        = LoadAdapter("Spek.Hosting.WindowsService", "net10.0-windows");
            var facType    = asm.GetType("Spek.Hosting.WindowsService.SpekWindowsServiceFactories")!;
            var facObj     = Activator.CreateInstance(facType)!;
            facType.GetProperty("ShutdownFactory")!.SetValue(facObj, shutdown);
            if (pause   is not null) facType.GetProperty("PauseFactory")!.SetValue(facObj, pause);
            if (cont    is not null) facType.GetProperty("ContinueFactory")!.SetValue(facObj, cont);
            if (power   is not null) facType.GetProperty("PowerEventFactory")!.SetValue(facObj, power);
            if (session is not null) facType.GetProperty("SessionChangeFactory")!.SetValue(facObj, session);
            if (custom  is not null) facType.GetProperty("CustomCommandFactory")!.SetValue(facObj, custom);

            var closed = asm.GetType("Spek.Hosting.WindowsService.SpekWindowsHostedService`1")!
                            .MakeGenericType(typeof(WinSpy));
            var svc    = Activator.CreateInstance(closed, facObj, grace)!;
            return new WinHost(svc, closed);
        }

        public Task StartAsync()       => (Task)_t.GetMethod("StartAsync")!.Invoke(_svc, new object?[] { CancellationToken.None })!;
        public Task StopAsync(CancellationToken ct = default)
                                       => (Task)_t.GetMethod("StopAsync")!.Invoke(_svc, new object?[] { ct })!;
        public void Pause()            => _t.GetMethod("Pause")!.Invoke(_svc, null);
        public void Continue()         => _t.GetMethod("Continue")!.Invoke(_svc, null);
        public void PowerEvent(string e) => _t.GetMethod("PowerEvent")!.Invoke(_svc, new object?[] { e });
        public void Custom(int code)   => _t.GetMethod("CustomCommand")!.Invoke(_svc, new object?[] { code });
        public ActorRef Entry          => (ActorRef)_t.GetProperty("Entry")!.GetValue(_svc)!;
        public ValueTask DisposeAsync()=> (ValueTask)_t.GetMethod("DisposeAsync")!.Invoke(_svc, null)!;
    }

    [Fact]
    public async Task Windows_PauseAndContinue_RouteMappedMessages()
    {
        WinSpy.Box.Clear();
        var host = WinHost.Create(
            shutdown: () => new Shutdown(),
            pause:    () => new Pause(),
            cont:     () => new Continue(),
            grace:    TimeSpan.FromSeconds(2));

        await host.StartAsync();
        host.Pause();
        host.Continue();

        Assert.True(WaitFor(() => WinSpy.Box.Count >= 2, TimeSpan.FromSeconds(3)),
            $"Expected Pause + Continue to be routed; saw {WinSpy.Box.Count}.");
        var got = WinSpy.Box.ToArray();
        Assert.Contains(got, m => m is Pause);
        Assert.Contains(got, m => m is Continue);

        await host.DisposeAsync();
    }

    [Fact]
    public async Task Windows_CustomCommand_ThreadsCodeThroughFactory()
    {
        WinSpy.Box.Clear();
        var host = WinHost.Create(
            shutdown: () => new Shutdown(),
            custom:   code => new Custom(code),
            grace:    TimeSpan.FromSeconds(2));

        await host.StartAsync();
        host.Custom(177);   // SCM custom control codes live in 128..255

        Assert.True(WaitFor(() => WinSpy.Box.Count >= 1, TimeSpan.FromSeconds(3)),
            "CustomCommand should route a Custom message.");
        var custom = Assert.IsType<Custom>(WinSpy.Box.Single());
        Assert.Equal(177, custom.Code);   // the int must be threaded through verbatim

        await host.DisposeAsync();
    }

    [Fact]
    public async Task Windows_NullFactoryHooks_AreSilentNoOps()
    {
        WinSpy.Box.Clear();
        // Only ShutdownFactory supplied — Pause / PowerEvent have no factory.
        var host = WinHost.Create(shutdown: () => new Shutdown(), grace: TimeSpan.FromSeconds(2));

        await host.StartAsync();

        // Both must be silent no-ops: no throw, no message delivered.
        var ex = Record.Exception(() => { host.Pause(); host.PowerEvent("Suspend"); });
        Assert.Null(ex);

        // Give any (erroneous) delivery a chance to land, then assert none did.
        await Task.Delay(150);
        Assert.Empty(WinSpy.Box);

        await host.DisposeAsync();
    }

    [Fact]
    public async Task Windows_StopAsync_RoutesShutdownToEntry()
    {
        WinSpy.Box.Clear();
        var host = WinHost.Create(shutdown: () => new Shutdown(), grace: TimeSpan.FromSeconds(2));

        await host.StartAsync();
        await host.StopAsync();

        Assert.True(WaitFor(() => WinSpy.Box.Count >= 1, TimeSpan.FromSeconds(3)),
            "StopAsync should Tell the entry actor its Shutdown message.");
        Assert.Contains(WinSpy.Box.ToArray(), m => m is Shutdown);

        await host.DisposeAsync();
    }

    // ================================================================
    //  Systemd adapter (reflection-loaded real production code).
    // ================================================================

    private sealed class SysSpy : SpyActorBase
    {
        public static readonly ConcurrentQueue<object> Box = new();
        protected override ConcurrentQueue<object> Sink => Box;
    }

    private sealed class SystemdHost
    {
        private readonly object _svc;
        private readonly Type _t;
        private SystemdHost(object svc, Type t) { _svc = svc; _t = t; }

        public static SystemdHost Create(
            Func<object> shutdown, Func<object>? reload = null, TimeSpan? grace = null)
        {
            var asm    = LoadAdapter("Spek.Hosting.Systemd", "net10.0");
            var closed = asm.GetType("Spek.Hosting.Systemd.SpekSystemdHostedService`1")!
                            .MakeGenericType(typeof(SysSpy));
            var svc    = Activator.CreateInstance(closed, shutdown, reload, grace)!;
            return new SystemdHost(svc, closed);
        }

        public Task StartAsync() => (Task)_t.GetMethod("StartAsync")!.Invoke(_svc, new object?[] { CancellationToken.None })!;
        public Task StopAsync(CancellationToken ct = default)
                                  => (Task)_t.GetMethod("StopAsync")!.Invoke(_svc, new object?[] { ct })!;
        public ValueTask DisposeAsync() => (ValueTask)_t.GetMethod("DisposeAsync")!.Invoke(_svc, null)!;
    }

    [Fact]
    public async Task Systemd_StopAsync_RoutesShutdownAndDisposesCleanly()
    {
        SysSpy.Box.Clear();
        var host = SystemdHost.Create(
            shutdown: () => new Shutdown(),
            reload:   () => new Reload(),
            grace:    TimeSpan.FromSeconds(2));

        await host.StartAsync();
        await host.StopAsync();

        Assert.True(WaitFor(() => SysSpy.Box.Count >= 1, TimeSpan.FromSeconds(3)),
            "systemd StopAsync should Tell the entry actor its Shutdown message.");
        Assert.Contains(SysSpy.Box.ToArray(), m => m is Shutdown);

        // DisposeAsync must tear down the SIGHUP registration + system
        // without throwing, and be idempotent.
        await host.DisposeAsync();
        var second = await Record.ExceptionAsync(async () => await host.DisposeAsync());
        Assert.Null(second);
    }

    // ================================================================
    //  Launchd adapter (reflection-loaded real production code).
    // ================================================================

    private sealed class LaunchSpy : SpyActorBase
    {
        public static readonly ConcurrentQueue<object> Box = new();
        protected override ConcurrentQueue<object> Sink => Box;
    }

    /// <summary>An entry actor that ignores everything — for the
    /// grace / cancellation-bound tests where we must NOT stop.</summary>
    private sealed class LaunchDeafActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    private sealed class LaunchdHost
    {
        private readonly object _svc;
        private readonly Type _t;
        private LaunchdHost(object svc, Type t) { _svc = svc; _t = t; }

        public static LaunchdHost Create(Type actorType, Func<object> shutdown, TimeSpan? grace = null)
        {
            var asm    = LoadAdapter("Spek.Hosting.Launchd", "net10.0");
            var closed = asm.GetType("Spek.Hosting.Launchd.SpekLaunchdHostedService`1")!
                            .MakeGenericType(actorType);
            var svc    = Activator.CreateInstance(closed, shutdown, grace)!;
            return new LaunchdHost(svc, closed);
        }

        public Task StartAsync() => (Task)_t.GetMethod("StartAsync")!.Invoke(_svc, new object?[] { CancellationToken.None })!;
        public Task StopAsync(CancellationToken ct = default)
                                  => (Task)_t.GetMethod("StopAsync")!.Invoke(_svc, new object?[] { ct })!;
        public ValueTask DisposeAsync() => (ValueTask)_t.GetMethod("DisposeAsync")!.Invoke(_svc, null)!;
    }

    [Fact]
    public async Task Launchd_StopAsync_RoutesShutdownToEntry()
    {
        LaunchSpy.Box.Clear();
        var host = LaunchdHost.Create(typeof(LaunchSpy), () => new Shutdown(), TimeSpan.FromSeconds(2));

        await host.StartAsync();
        await host.StopAsync();

        Assert.True(WaitFor(() => LaunchSpy.Box.Count >= 1, TimeSpan.FromSeconds(3)),
            "launchd StopAsync should Tell the entry actor its Shutdown message.");
        Assert.Contains(LaunchSpy.Box.ToArray(), m => m is Shutdown);

        await host.DisposeAsync();
    }

    [Fact]
    public async Task Launchd_StopAsync_HonorsExternalCancellationToken_OverLongGrace()
    {
        // A deaf actor + a huge grace: only the external cancellation token
        // can end StopAsync's wait loop. This is the same contract the
        // Generic Host's ShutdownTimeout relies on.
        var host = LaunchdHost.Create(
            typeof(LaunchDeafActor), () => new Shutdown(), TimeSpan.FromSeconds(60));

        await host.StartAsync();

        using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));
        var sw = Stopwatch.StartNew();
        await host.StopAsync(cts.Token);
        sw.Stop();

        // Must return via the cancellation branch, not the 60s grace.
        // Generous ceiling for parallel-load tolerance; still unmistakably
        // shorter than the 60s window.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(30),
            $"StopAsync should short-circuit on the cancelled token, but took {sw.Elapsed} (grace was 60s).");

        await host.DisposeAsync();
    }

    [Fact]
    public async Task Launchd_StopAsync_BoundedByGrace_WhenActorIgnoresShutdown()
    {
        // Contrast case: no external cancellation, short grace, deaf actor.
        // Proves the grace deadline is the bound when the token never fires
        // (so the test above is genuinely measuring the token branch).
        var host = LaunchdHost.Create(
            typeof(LaunchDeafActor), () => new Shutdown(), TimeSpan.FromMilliseconds(200));

        await host.StartAsync();

        var sw = Stopwatch.StartNew();
        await host.StopAsync(CancellationToken.None);
        sw.Stop();

        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(30),
            $"StopAsync should return via the 200ms grace deadline, but took {sw.Elapsed}.");

        await host.DisposeAsync();
    }
}

