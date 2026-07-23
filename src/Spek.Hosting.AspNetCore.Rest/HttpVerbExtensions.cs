namespace Spek.Hosting.AspNetCore.Rest;

// HttpVerb (the enum) is authored in Spek — see HttpVerb.spek. This file keeps
// the extension helper, which Spek doesn't express (a `this`-parameter method).

/// <summary>
/// Conversion helpers between <see cref="HttpVerb"/> and ASP.NET
/// Core's string-based method names. Internal — users interact with
/// the typed enum exclusively.
/// </summary>
internal static class HttpVerbExtensions
{
    public static string ToHttpMethodString(this HttpVerb verb) => verb switch
    {
        HttpVerb.Get     => "GET",
        HttpVerb.Post    => "POST",
        HttpVerb.Put     => "PUT",
        HttpVerb.Patch   => "PATCH",
        HttpVerb.Delete  => "DELETE",
        HttpVerb.Head    => "HEAD",
        HttpVerb.Options => "OPTIONS",
        _ => throw new ArgumentOutOfRangeException(nameof(verb), verb, null),
    };
}
