namespace Spek.Testing;

/// <summary>
/// Marks a Spek native test (emitted from a <c>test "..." { }</c> block) as an
/// xUnit fact, so it is discovered and run by <c>dotnet test</c>, IDE test
/// explorers, and CI with no special tooling.
///
/// <para>
/// The Spek emitter stamps the framework-neutral <c>[Spek.Testing.SpekTest]</c>
/// attribute on each generated test method; referencing this package binds that
/// name to xUnit. Because it derives from <see cref="Xunit.FactAttribute"/>,
/// xUnit's default discovery picks it up — no custom test-case discoverer is
/// needed. A future <c>Spek.Testing.NUnit</c> adapter binds the same attribute
/// name to an NUnit test instead; the emitter stays framework-agnostic.
/// </para>
///
/// <para>The <c>DisplayName</c> (the test description) is inherited from
/// <see cref="Xunit.FactAttribute"/>.</para>
/// </summary>
public sealed class SpekTestAttribute : Xunit.FactAttribute
{
}
