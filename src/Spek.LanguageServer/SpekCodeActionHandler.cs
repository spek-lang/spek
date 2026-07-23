using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/codeAction</c> requests — quick-fix suggestions
/// shown in the editor's lightbulb / "fix this" UI. Each diagnostic in
/// the request range is matched against a known CE-code fix provider; if
/// a fix exists, a <see cref="CodeAction"/> with a ready-to-apply
/// <see cref="WorkspaceEdit"/> is returned.
///
/// Ships fixers for the most common CE codes that have an obvious
/// remediation:
///
/// <list type="bullet">
///   <item><b>CE0011</b> — <c>become UnknownBehavior</c> — suggests
///         renaming to the closest-match behavior name on the
///         enclosing actor (Levenshtein distance ≤ 3).</item>
///   <item><b>CE0091</b> — actor inherits unknown channel — suggests
///         declaring the missing channel as a stub.</item>
///   <item><b>CE0093</b> — channel inherits unknown channel — suggests
///         declaring the missing parent channel.</item>
/// </list>
///
/// Other CE codes (CE0014 unused behavior, CE0010 immutable-type
/// violations, etc.) are deferred to a later iteration once usage
/// patterns clarify which fixes are most asked for.
/// </summary>
internal sealed class SpekCodeActionHandler : CodeActionHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekCodeActionHandler(DocumentCache cache) => _cache = cache;

    protected override CodeActionRegistrationOptions CreateRegistrationOptions(
        CodeActionCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            ResolveProvider  = false,
        };

    public override Task<CommandOrCodeActionContainer?> Handle(
        CodeActionParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null)
            return Task.FromResult<CommandOrCodeActionContainer?>(null);

        var actions = new List<CommandOrCodeAction>();
        if (request.Context.Diagnostics is null)
            return Task.FromResult<CommandOrCodeActionContainer?>(actions);

        foreach (var diag in request.Context.Diagnostics)
        {
            switch (diag.Code?.String)
            {
                case "CE0011":
                    actions.AddRange(BuildCe0011Fixes(diag, entry.Tree, request.TextDocument.Uri));
                    break;
                case "CE0091":
                case "CE0093":
                    actions.AddRange(BuildMissingChannelFixes(diag, entry.Tree, request.TextDocument.Uri));
                    break;
                case "CE0083":
                    actions.AddRange(BuildBlockingCallFixes(diag, request.TextDocument.Uri));
                    break;
                case "CE0115":
                    actions.AddRange(BuildSyncIoFixes(diag, request.TextDocument.Uri));
                    break;
            }
        }

        return Task.FromResult<CommandOrCodeActionContainer?>(actions);
    }

    public override Task<CodeAction> Handle(
        CodeAction request, CancellationToken cancellationToken) =>
        Task.FromResult(request);  // resolve = no-op since we return full edits up front

    /// <summary>
    /// Static-call name → (replacement, title) for the CE0083 blocking calls
    /// that have a clean async equivalent the invisible-async pass then awaits.
    /// CE0083 fires on these only in the bare `Type.Method` form, so the source
    /// starts with the matched name at the diagnostic range and we replace just
    /// that identifier, keeping the argument list. (`Console.ReadLine`, instance
    /// `.WaitAny()` etc. have no value-preserving rewrite — no fix offered.)
    /// </summary>
    private static readonly Dictionary<string, (string Replacement, string Title)> BlockingCallRewrites =
        new(StringComparer.Ordinal)
        {
            ["Thread.Sleep"] = ("System.Threading.Tasks.Task.Delay",
                                "Replace 'Thread.Sleep' with 'Task.Delay' (non-blocking, awaited)"),
            ["Task.WaitAll"] = ("Task.WhenAll",
                                "Replace 'Task.WaitAll' with 'Task.WhenAll' (awaited)"),
        };

    private static IEnumerable<CommandOrCodeAction> BuildBlockingCallFixes(
        Diagnostic diag, DocumentUri uri)
    {
        // The diagnostic message is `'{Type.Method}' …`; pull the name out and
        // see if we have a rewrite for it.
        var msg = diag.Message;
        if (msg is null || msg.Length < 2 || msg[0] != '\'') yield break;
        var end = msg.IndexOf('\'', 1);
        if (end < 0) yield break;
        var name = msg.Substring(1, end - 1);
        if (!BlockingCallRewrites.TryGetValue(name, out var rewrite)) yield break;

        var start = diag.Range.Start;
        var replaceRange = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
            start,
            new Position(start.Line, start.Character + name.Length));

        yield return new CommandOrCodeAction(new CodeAction
        {
            Title = rewrite.Title,
            Kind  = CodeActionKind.QuickFix,
            Diagnostics = new Container<Diagnostic>(diag),
            Edit = new WorkspaceEdit
            {
                Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
                {
                    [uri] = new[] { new TextEdit { Range = replaceRange, NewText = rewrite.Replacement } }
                }
            }
        });
    }

    /// <summary>
    /// CE0115 fixer — synchronous BCL I/O (`File.ReadAllText` …) has a drop-in
    /// `*Async` sibling. The diagnostic fires on the bare `Type.Method` form,
    /// so the source starts with the matched name at the diagnostic range;
    /// the fix appends `Async` to it, and invisible async then awaits the
    /// resulting Task-returning call.
    /// </summary>
    private static IEnumerable<CommandOrCodeAction> BuildSyncIoFixes(
        Diagnostic diag, DocumentUri uri)
    {
        var msg = diag.Message;
        if (msg is null || msg.Length < 2 || msg[0] != '\'') yield break;
        var end = msg.IndexOf('\'', 1);
        if (end < 0) yield break;
        var name = msg.Substring(1, end - 1);

        var start = diag.Range.Start;
        var insertAt = new Position(start.Line, start.Character + name.Length);

        yield return new CommandOrCodeAction(new CodeAction
        {
            Title = $"Replace '{name}' with '{name}Async' (non-blocking, awaited)",
            Kind  = CodeActionKind.QuickFix,
            Diagnostics = new Container<Diagnostic>(diag),
            Edit = new WorkspaceEdit
            {
                Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
                {
                    // Insert the `Async` suffix right after the method name,
                    // leaving the argument list untouched.
                    [uri] = new[]
                    {
                        new TextEdit
                        {
                            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                                insertAt, insertAt),
                            NewText = "Async",
                        }
                    }
                }
            }
        });
    }

    /// <summary>
    /// CE0011 fixer — finds the actor enclosing the diagnostic, picks the
    /// behavior name that's closest to the offending <c>become</c> target,
    /// and offers it as a one-click rename.
    /// </summary>
    private static IEnumerable<CommandOrCodeAction> BuildCe0011Fixes(
        Diagnostic diag, SpekFile file, DocumentUri uri)
    {
        var line   = (int)diag.Range.Start.Line + 1;
        var column = (int)diag.Range.Start.Character + 1;
        var chain  = PositionResolver.FindChain(file, line, column);

        var becomeStmt    = chain.OfType<BecomeStmt>().LastOrDefault();
        var enclosingActor = chain.OfType<ActorDecl>().FirstOrDefault();
        if (becomeStmt is null || enclosingActor is null) yield break;

        var candidates = enclosingActor.Members
            .OfType<BehaviorDecl>()
            .Select(b => b.Name)
            .Where(n => Levenshtein(n, becomeStmt.BehaviorName) <= 3)
            .OrderBy(n => Levenshtein(n, becomeStmt.BehaviorName))
            .Take(3)
            .ToList();

        foreach (var candidate in candidates)
        {
            yield return new CommandOrCodeAction(new CodeAction
            {
                Title = $"Rename to '{candidate}'",
                Kind  = CodeActionKind.QuickFix,
                Diagnostics = new Container<Diagnostic>(diag),
                Edit = new WorkspaceEdit
                {
                    Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
                    {
                        [uri] = new[]
                        {
                            new TextEdit
                            {
                                Range   = diag.Range,
                                NewText = $"become {candidate};",
                            }
                        },
                    },
                },
            });
        }
    }

    /// <summary>
    /// CE0091 / CE0093 fixer — offers to declare the missing channel as a
    /// stub at the bottom of the file. The user gets a starting point;
    /// they fill in the inputs/emits.
    /// </summary>
    private static IEnumerable<CommandOrCodeAction> BuildMissingChannelFixes(
        Diagnostic diag, SpekFile file, DocumentUri uri)
    {
        // Pick the MISSING channel name out of the message by CODE + position —
        // both messages also quote a "'channel X { ... }'" suggestion, so neither
        // first-nor-last alone is reliable:
        //   CE0091: "Unknown channel or base actor 'X' in actor 'Y'..." → X is FIRST.
        //   CE0093: "Channel 'Y' inherits from unknown channel 'X'..."   → X is SECOND.
        var msg = diag.Message;
        var quoted = new List<string>();
        for (int i = msg.IndexOf('\''); i >= 0 && quoted.Count < 3;)
        {
            var j = msg.IndexOf('\'', i + 1);
            if (j < 0) break;
            quoted.Add(msg[(i + 1)..j]);
            i = msg.IndexOf('\'', j + 1);
        }
        var missing = diag.Code?.String == "CE0093"
            ? (quoted.Count > 1 ? quoted[1] : null)
            : (quoted.Count > 0 ? quoted[0] : null);
        if (string.IsNullOrEmpty(missing)) yield break;

        // Append the stub at the end of the file. Track the file's
        // last-line span so we can pick an insertion position.
        var endLine = (int)file.Span.EndLine;

        yield return new CommandOrCodeAction(new CodeAction
        {
            Title = $"Declare 'channel {missing}'",
            Kind  = CodeActionKind.QuickFix,
            Diagnostics = new Container<Diagnostic>(diag),
            Edit = new WorkspaceEdit
            {
                Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
                {
                    [uri] = new[]
                    {
                        new TextEdit
                        {
                            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                                new Position(endLine, 0),
                                new Position(endLine, 0)),
                            NewText = $"\nchannel {missing}\n{{\n    // TODO: declare inputs and emits\n}}\n",
                        }
                    },
                },
            },
        });
    }

    /// <summary>
    /// Damerau-Levenshtein distance — for "did you mean?" candidate ranking.
    /// Keeps things tight (max 3-edit distance) so we don't suggest wildly
    /// unrelated names.
    /// </summary>
    private static int Levenshtein(string a, string b)
    {
        if (string.IsNullOrEmpty(a)) return b?.Length ?? 0;
        if (string.IsNullOrEmpty(b)) return a.Length;

        var d = new int[a.Length + 1, b.Length + 1];
        for (int i = 0; i <= a.Length; i++) d[i, 0] = i;
        for (int j = 0; j <= b.Length; j++) d[0, j] = j;

        for (int i = 1; i <= a.Length; i++)
        for (int j = 1; j <= b.Length; j++)
        {
            int cost = a[i - 1] == b[j - 1] ? 0 : 1;
            d[i, j] = Math.Min(
                Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                d[i - 1, j - 1] + cost);
        }
        return d[a.Length, b.Length];
    }
}
