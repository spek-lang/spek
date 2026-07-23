using MediatR;
using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;

namespace Spek.LanguageServer;

/// <summary>Request payload for <c>spek/emitCSharp</c>: the document to transpile.</summary>
[Method("spek/emitCSharp", Direction.ClientToServer)]
public record EmitCSharpParams : IRequest<EmitCSharpResult>
{
    public string Uri { get; init; } = string.Empty;
}

/// <summary>Either the emitted C# (<see cref="CSharp"/>) or why it couldn't be produced.</summary>
public record EmitCSharpResult
{
    public string? CSharp { get; init; }
    public string? Error  { get; init; }
}

/// <summary>
/// Custom <c>spek/emitCSharp</c> request: given an open document, returns the C#
/// the compiler would generate for it. Backs the experimental "Show emitted C#"
/// command in the VS Code extension — a peek at the transpiler output for the
/// curious, deliberately gated behind a setting so it stays a diagnostic aid
/// rather than something workflows come to depend on. Reuses the same
/// parse-then-emit path as <c>spekc compile</c>.
/// </summary>
internal sealed class SpekEmitCSharpHandler : IJsonRpcRequestHandler<EmitCSharpParams, EmitCSharpResult>
{
    private readonly DocumentCache _cache;

    public SpekEmitCSharpHandler(DocumentCache cache) => _cache = cache;

    public Task<EmitCSharpResult> Handle(EmitCSharpParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(DocumentUri.From(request.Uri));
        if (entry is null)
            return DoneAsync(error: "That document isn't open in the language server.");

        var result = SpekCompiler.Parse(entry.Source);
        if (result.Tree is null || !result.Success)
            return DoneAsync(error: "The source has errors — fix the diagnostics to see the emitted C#.");

        try
        {
            var csharp = new FileEmitter().Emit(result.Tree, emitTests: true);
            return DoneAsync(csharp: csharp);
        }
        catch (Exception ex)
        {
            return DoneAsync(error: $"Emit failed: {ex.Message}");
        }
    }

    private static Task<EmitCSharpResult> DoneAsync(string? csharp = null, string? error = null) =>
        Task.FromResult(new EmitCSharpResult { CSharp = csharp, Error = error });
}
