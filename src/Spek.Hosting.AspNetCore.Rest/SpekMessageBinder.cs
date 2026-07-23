using System.Reflection;
using System.Text.Json;
using Microsoft.AspNetCore.Http;

namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// Constructs a Spek message instance from the current
/// HTTP request, binding constructor parameters by name from the
/// path values, query string, or JSON body. The convention:
/// <list type="bullet">
///   <item>Parameter names that match a path placeholder bind from
///         <c>RouteValues</c>.</item>
///   <item>For body-bearing verbs (POST, PUT, PATCH), the rest of
///         the parameters bind from the JSON request body.</item>
///   <item>For body-less verbs (GET, DELETE, HEAD, OPTIONS), the
///         rest bind from the query string. Default-valued
///         parameters fall back to their defaults when the query
///         doesn't contain the value.</item>
/// </list>
/// Type conversion uses <see cref="Convert.ChangeType(object?, Type)"/>
/// for primitives plus <see cref="JsonSerializer"/> for record
/// payloads.
/// </summary>
internal static class SpekMessageBinder
{
    public static async ValueTask<object> BindAsync(HttpContext http, Type messageType)
    {
        var ctor = messageType.GetConstructors().FirstOrDefault()
            ?? throw new InvalidOperationException(
                $"Message type {messageType.Name} has no constructor — Spek " +
                $"messages should compile to records with a primary constructor.");

        var parameters = ctor.GetParameters();
        var args = new object?[parameters.Length];

        var hasBody = HttpMethods.IsPost(http.Request.Method)
                   || HttpMethods.IsPut(http.Request.Method)
                   || HttpMethods.IsPatch(http.Request.Method);

        // Read body once if needed; bind named parameters out of it.
        // ContentLength is null/-1 when the request is chunked
        // (HttpClient.PostAsJsonAsync uses chunked for small bodies),
        // so we attempt the read whenever the verb expects a body and
        // the content type is JSON-shaped.
        Dictionary<string, JsonElement>? bodyJson = null;
        if (hasBody && IsJsonRequest(http.Request))
        {
            try
            {
                bodyJson = await JsonSerializer.DeserializeAsync<Dictionary<string, JsonElement>>(
                    http.Request.Body,
                    new JsonSerializerOptions
                    {
                        PropertyNameCaseInsensitive = true,
                    },
                    http.RequestAborted) ?? new Dictionary<string, JsonElement>();
            }
            catch (JsonException)
            {
                bodyJson = new Dictionary<string, JsonElement>();
            }
        }

        for (int i = 0; i < parameters.Length; i++)
        {
            var p = parameters[i];

            // 1. Path values (route placeholders)
            if (http.Request.RouteValues.TryGetValue(p.Name!, out var routeValue) && routeValue is not null)
            {
                args[i] = ConvertSimple(routeValue, p.ParameterType);
                continue;
            }

            // 2. Body fields (only on body-bearing verbs)
            if (hasBody && bodyJson is not null && bodyJson.TryGetValue(p.Name!, out var bodyValue))
            {
                args[i] = bodyValue.Deserialize(p.ParameterType, new JsonSerializerOptions
                {
                    PropertyNameCaseInsensitive = true,
                });
                continue;
            }

            // 3. Query string
            if (http.Request.Query.TryGetValue(p.Name!, out var queryValue))
            {
                args[i] = ConvertSimple((string)queryValue!, p.ParameterType);
                continue;
            }

            // 4. Default value if the parameter has one.
            if (p.HasDefaultValue) { args[i] = p.DefaultValue; continue; }

            // 5. Best-effort default(T) for value types; null for ref types.
            args[i] = p.ParameterType.IsValueType
                ? Activator.CreateInstance(p.ParameterType)
                : null;
        }

        return ctor.Invoke(args)!;
    }

    private static bool IsJsonRequest(HttpRequest request)
    {
        // Default to "yes, try to read it as JSON" if the client
        // didn't send a content type — most HTTP clients in test
        // suites do this. Reject only when the type is explicitly
        // not JSON-shaped (form data, plain text, etc.).
        var ct = request.ContentType;
        if (string.IsNullOrEmpty(ct)) return true;
        return ct.Contains("json", StringComparison.OrdinalIgnoreCase);
    }

    private static object? ConvertSimple(object value, Type targetType)
    {
        if (value is null) return null;
        var t = Nullable.GetUnderlyingType(targetType) ?? targetType;
        if (t.IsInstanceOfType(value)) return value;
        if (t == typeof(string))       return value.ToString();
        try
        {
            return Convert.ChangeType(value, t, System.Globalization.CultureInfo.InvariantCulture);
        }
        catch
        {
            return targetType.IsValueType ? Activator.CreateInstance(targetType) : null;
        }
    }
}
