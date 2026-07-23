using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Guards the <c>Spek.Templates</c> template file against grammar drift.
/// If this test fails, the template contains syntax the parser no longer
/// accepts — fix the template (or the parser regression) before shipping.
/// </summary>
public class TemplateFixtureParseTest
{
    [Fact]
    public void SpekConsoleTemplate_GreeterSpek_ParsesCleanly()
    {
        // Inline mirror of the template's Greeter.spek. Keep in sync.
        const string greeterSource = """
            namespace SpekHelloWorld;

            using System;

            message Greet(string name);
            message Farewell();

            actor Greeter
            {
                behavior Listening
                {
                    on Greet g =>
                    {
                        Console.WriteLine($"Hello, {g.name}!");
                    }

                    on Farewell =>
                        Console.WriteLine("Goodbye.");
                }
            }

            program Main
            {
                var system = new ActorSystem("hello");
                ActorRef greeter = system.Spawn<Greeter>();

                greeter.Tell(new Greet("world"));
                greeter.Tell(new Farewell());

                system.AwaitTermination();
            }
            """;

        var result = SpekCompiler.Parse(greeterSource);

        Assert.True(result.Success,
            "Template Greeter.spek failed to parse:\n" +
            string.Join("\n", result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
    }
}
