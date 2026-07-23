using Newtonsoft.Json.Linq;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/codeLens</c> "▶ Run test" actions on Spek's
/// convention-based tests: a top-level <c>class</c> or <c>module</c> whose name
/// ends in <c>Tests</c> is a test container, and each of its parameterless public
/// methods is a single test. The container gets a "Run all tests" lens; each test
/// method gets a "Run test" lens. Both carry the <c>spek.runTest</c> command with
/// a <c>FullyQualifiedName~…</c> filter and the file path, which the VS Code
/// extension turns into a <c>dotnet test</c> run in the enclosing project.
/// </summary>
internal sealed class SpekCodeLensHandler : CodeLensHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekCodeLensHandler(DocumentCache cache) => _cache = cache;

    protected override CodeLensRegistrationOptions CreateRegistrationOptions(
        CodeLensCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            ResolveProvider  = false,
        };

    public override Task<CodeLensContainer?> Handle(
        CodeLensParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        var tree  = entry?.Tree ?? entry?.LastGoodTree;
        if (tree is null) return Task.FromResult<CodeLensContainer?>(null);

        var ns     = tree.Namespace?.Name.ToString();
        var path   = request.TextDocument.Uri.GetFileSystemPath();
        var lenses = new List<CodeLens>();

        foreach (var (name, span, methods) in TestContainers(tree))
        {
            var containerFilter = ns is null ? name : $"{ns}.{name}";
            lenses.Add(Lens(span, "▶ Run all tests", containerFilter, path));
            foreach (var m in methods)
                lenses.Add(Lens(m.Span, "▶ Run test", $"{containerFilter}.{m.Name}", path));
        }

        return Task.FromResult<CodeLensContainer?>(new CodeLensContainer(lenses));
    }

    // ResolveProvider is false, so lenses arrive fully built; echo any back.
    public override Task<CodeLens> Handle(CodeLens request, CancellationToken cancellationToken) =>
        Task.FromResult(request);

    private static IEnumerable<(string Name, SourceSpan Span, IReadOnlyList<MethodDecl> Methods)>
        TestContainers(SpekFile tree)
    {
        foreach (var decl in tree.Declarations)
        {
            switch (decl)
            {
                case ClassDecl c when IsTestName(c.Name):
                    yield return (c.Name, c.Span, TestMethods(c.Methods));
                    break;
                case ModuleDecl m when IsTestName(m.Name):
                    yield return (m.Name, m.Span, TestMethods(m.Methods));
                    break;
            }
        }
    }

    private static bool IsTestName(string name) => name.EndsWith("Tests", StringComparison.Ordinal);

    private static IReadOnlyList<MethodDecl> TestMethods(IEnumerable<MethodDecl> methods) =>
        methods.Where(m => m.Visibility == Visibility.Public && m.Parameters.Count == 0).ToList();

    private static CodeLens Lens(SourceSpan span, string title, string filter, string path) => new()
    {
        Range   = new Range(new Position(span.StartLine - 1, 0), new Position(span.StartLine - 1, 0)),
        Command = new Command
        {
            Title     = title,
            Name      = "spek.runTest",
            Arguments = new JArray { filter, path },
        },
    };
}
