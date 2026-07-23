using Spek.Compiler.AST;

namespace Spek.Compiler.Emit;

/// <summary>
/// Translates Spek AST statements to C# statement lines.
/// Delegates expression sub-trees to <see cref="ExpressionEmitter"/>.
/// </summary>
public sealed class StatementEmitter
{
    private readonly CSharpWriter _w;
    private readonly ExpressionEmitter _expr;

    public StatementEmitter(CSharpWriter writer, ExpressionEmitter exprEmitter)
    {
        _w    = writer;
        _expr = exprEmitter;
    }

    /// <summary>
    /// When true, <c>return expr;</c> inside this context is the Spek
    /// reply idiom: the expression is sent to the current sender via
    /// <c>_sender.Tell(...)</c> (the dispatch-local sender — never the
    /// shared field, which concurrent readers would race on) and then the handler returns
    /// control. Set by <see cref="ActorEmitter"/> only while it's
    /// generating the body of an <c>on</c> handler in a
    /// <see cref="BehaviorDecl"/>. Methods, constructors, and lifecycle
    /// hooks stay in the standard "return is just return" mode.
    /// </summary>
    public bool InsideOnHandler { get; set; }

    /// <summary>
    /// Sets the in-scope param/binding names for the body about to be emitted, so the
    /// expression emitter won't `_`-prefix a name that shadows an actor field. Locals
    /// are added incrementally as they're declared.
    /// </summary>
    public void SetShadowed(IEnumerable<string> names) => _expr.SetShadowed(names);

    // Block nesting depth — when source mapping is on, the outermost block
    // exit restores `#line default` so trailing generated scaffolding isn't
    // attributed to the last .spek statement.
    private int _blockDepth;

    public void EmitBlock(BlockStmt block)
    {
        _w.Line("{");
        _w.Indent();
        _blockDepth++;
        foreach (var stmt in block.Statements)
            EmitStatement(stmt);
        _blockDepth--;
        _w.Dedent();
        _w.Line("}");
        if (_blockDepth == 0) _w.MapDefault();
    }

    public void EmitStatement(Stmt stmt)
    {
        // Attribute this statement's emitted line(s) to its .spek line
        // (no-op unless the writer has source mapping enabled).
        _w.MapLine(stmt.Span.StartLine);
        switch (stmt)
        {
            case BecomeStmt b:
                _w.Line($"_behavior = {b.BehaviorName}_HandleAsync;");
                // Swap the reader/writer classifier alongside the
                // dispatch delegate so the slot's per-message reader-vs-writer
                // decision tracks the new behavior.
                _w.Line($"_isReaderClassifier = {b.BehaviorName}_IsReaderMessage;");
                break;

            case PersistStmt:
                _w.Line("await PersistAsync();");
                break;

            case ReturnStmt r when r.Value is null:
                _w.Line("return;");
                break;

            case ReturnStmt r when InsideOnHandler:
                // Option D: `return expr;` inside an on-handler sends
                // the expression back to whoever sent the message, then
                // exits the handler. Callers using `ask` receive this
                // value via AskAsync<TReply>; callers using Tell ignore
                // the reply (the sender ref points at the dead-letter
                // sink for a Tell without a real reply-to).
                // The reply carries this actor as its sender (implicit-sender
                // convention) so the asker/teller can itself reply onward.
                _w.Line($"_sender.Tell({_expr.Emit(r.Value!)}, _selfRef);");
                _w.Line("return;");
                break;

            case ReturnStmt r:
                _w.Line($"return {_expr.Emit(r.Value!)};");
                break;

            case VarDeclStmt v:
                var typeName = v.Type != null ? v.Type.ToString() : "var";
                var usingKw  = v.IsUsing ? "using " : "";
                _w.Line($"{usingKw}{typeName} {v.Name} = {_expr.Emit(v.Initializer)};");
                _expr.AddShadowed(v.Name);   // a local shadows a same-named field from here on
                break;

            case SwitchStmt sw:
                _w.Line($"switch ({_expr.Emit(sw.Subject)})");
                _w.Line("{");
                _w.Indent();
                foreach (var section in sw.Sections)
                {
                    foreach (var label in section.Labels)
                    {
                        if (label.Pattern is null)
                            _w.Line("default:");
                        else
                        {
                            var guard = label.Guard is null ? "" : $" when {_expr.Emit(label.Guard)}";
                            _w.Line($"case {_expr.EmitPattern(label.Pattern)}{guard}:");
                        }
                    }
                    _w.Indent();
                    foreach (var st in section.Body) EmitStatement(st);
                    _w.Dedent();
                }
                _w.Dedent();
                _w.Line("}");
                break;

            case IfStmt ifStmt:
                EmitIf(ifStmt);
                break;

            case ForStmt forStmt:
                EmitFor(forStmt);
                break;

            case WhileStmt whileStmt:
                _w.Line($"while ({_expr.Emit(whileStmt.Condition)})");
                EmitBlock(whileStmt.Body);
                break;

            case ForeachStmt fe:
                var feType = fe.Type != null ? fe.Type.ToString() : "var";
                _w.Line($"foreach ({feType} {fe.Name} in {_expr.Emit(fe.Collection)})");
                _expr.AddShadowed(fe.Name);   // loop variable shadows a same-named field
                EmitBlock(fe.Body);
                break;

            case DoWhileStmt dw:
                _w.Line("do");
                EmitBlock(dw.Body);
                _w.Line($"while ({_expr.Emit(dw.Condition)});");
                break;

            case BreakStmt:
                _w.Line("break;");
                break;

            case ContinueStmt:
                _w.Line("continue;");
                break;

            case ExpressionStmt e:
                _w.Line($"{_expr.Emit(e.Expr)};");
                break;

            case BlockStmt nested:
                EmitBlock(nested);
                break;

            case TryStmt tryStmt:
                EmitTry(tryStmt);
                break;

            case ThrowStmt t when t.Value is null:
                _w.Line("throw;");
                break;

            case ThrowStmt t:
                _w.Line($"throw {_expr.Emit(t.Value!)};");
                break;

            default:
                throw new NotSupportedException(
                    $"Cannot emit statement type {stmt.GetType().Name}");
        }
    }

    private void EmitTry(TryStmt tryStmt)
    {
        _w.Line("try");
        EmitBlock(tryStmt.Try);

        foreach (var c in tryStmt.Catches)
        {
            // catch clause shapes:
            //   catch                            → catch
            //   catch (T)                        → catch (T)
            //   catch (T name)                   → catch (T name)
            //   catch (T name when guard)        → catch (T name) when (guard)
            string header;
            if (c.ExceptionType is null)
            {
                header = "catch";
            }
            else
            {
                var exTypeName = c.ExceptionType.ToString();
                // No binding in the source → none in the emitted C#;
                // a synthesized name would just be unused (CS0168).
                header = c.Binding is null
                    ? $"catch ({exTypeName})"
                    : $"catch ({exTypeName} {c.Binding})";
                if (c.When is not null)
                    header += $" when ({_expr.Emit(c.When)})";
            }
            _w.Line(header);
            EmitBlock(c.Body);
        }

        if (tryStmt.Finally is not null)
        {
            _w.Line("finally");
            EmitBlock(tryStmt.Finally);
        }
    }

    private void EmitIf(IfStmt ifStmt)
    {
        _w.Line($"if ({_expr.Emit(ifStmt.Condition)})");
        EmitBlock(ifStmt.Then);

        if (ifStmt.Else is IfStmt elseIf)
        {
            _w.Append("else ");
            EmitIf(elseIf);
        }
        else if (ifStmt.Else is BlockStmt elseBlock)
        {
            _w.Line("else");
            EmitBlock(elseBlock);
        }
    }

    private void EmitFor(ForStmt forStmt)
    {
        var typeName = forStmt.Init.Type != null ? forStmt.Init.Type.ToString() : "var";
        var init = $"{typeName} {forStmt.Init.Name} = {_expr.Emit(forStmt.Init.Initializer)}";
        _expr.AddShadowed(forStmt.Init.Name);   // loop var shadows a same-named field in cond/inc/body
        var cond = _expr.Emit(forStmt.Condition);
        var inc  = _expr.Emit(forStmt.Increment);
        _w.Line($"for ({init}; {cond}; {inc})");
        EmitBlock(forStmt.Body);
    }
}
