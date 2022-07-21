using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace TestedMethodLister;

public class TestedMethodLister {

    public static void Main(string[] args) {


        var allTests = CSharpSyntaxTree.ParseText(File.ReadAllText(args[0]));
        CompilationUnitSyntax root = allTests.GetCompilationUnitRoot();

        var compilation = CSharpCompilation.Create("AllTests")
            .AddReferences(MetadataReference.CreateFromFile(
        typeof(object).Assembly.Location))
            .AddSyntaxTrees(allTests);

        SemanticModel model = compilation.GetSemanticModel(allTests);
        var myTypeSyntax = root.DescendantNodes().OfType<TypeDeclarationSyntax>().First();
        var myTypeInfo = model.GetDeclaredSymbol(myTypeSyntax);

        var nodes = root.DescendantNodes(n => true);
        HashSet<INamedTypeSymbol> typeSet = new();
        if (nodes != null)
        {
            var syntaxNodes = nodes as SyntaxNode[] ?? nodes.ToArray();

            // IdentifierNameSyntax:
            //  - var keyword
            //  - identifiers of any kind (including type names)
            var namedTypes = syntaxNodes
                .OfType<IdentifierNameSyntax>()
                .Select(id => model.GetSymbolInfo(id).Symbol)
                .OfType<INamedTypeSymbol>()
                .Select(type => type.OriginalDefinition)
                .ToHashSet();

            typeSet.UnionWith(namedTypes);

            // ExpressionSyntax:
            //  - method calls
            //  - property uses
            //  - field uses
            //  - all kinds of composite expressions
            var expressionTypes = syntaxNodes
                .OfType<ExpressionSyntax>()
                .Select(ma => model.GetTypeInfo(ma).Type)
                .OfType<INamedTypeSymbol>()
                .Select(type => type.OriginalDefinition)
                .ToHashSet();

            typeSet.UnionWith(expressionTypes);
        }

        TestedMethodWalker walker = new TestedMethodWalker(model, typeSet, compilation);
        walker.Visit(root);

        foreach (var method in walker.TestedMethodNames)
            System.Console.WriteLine(method);
    }
}

class TestedMethodWalker : CSharpSyntaxWalker {

    public HashSet<string> TestedMethodNames { get; }
    private SemanticModel model;
    private HashSet<INamedTypeSymbol> typeSet;
    private Compilation comp;

    public TestedMethodWalker(SemanticModel model, HashSet<INamedTypeSymbol> typeSet, Compilation comp) {
        TestedMethodNames = new();
        this.model = model;
        this.typeSet = typeSet;
        this.comp = comp;
    }

    public bool CheckIfInUnitTestsDecl(SyntaxNode startNode) {
        
        foreach (var node in startNode.Ancestors()) {
            if (node is NamespaceDeclarationSyntax ns) {
                if (ns.Name is IdentifierNameSyntax ident) {
                    if (ident.GetText().ToString().Contains("UnitTests")) 
                        return true;
                }
            } else if (node is ClassDeclarationSyntax cls) {
                if (cls.GetText().ToString().Contains("UnitTests"))
                    return true;
            }
        }        
        return false;
    }

    public SimpleNameSyntax? PullSimpleNameFromParenExpr(ParenthesizedExpressionSyntax startNode) {
        ExpressionSyntax node = startNode;
        while (node is ParenthesizedExpressionSyntax p) {
            if (p.Expression is SimpleNameSyntax s) {
                return s;
            }

            node = p.Expression;
        }
        return null;
    }

    public HashSet<INamedTypeSymbol> GetDerivedTypes(ITypeSymbol baseType) {
        HashSet<INamedTypeSymbol> derivedTypes = new();
        foreach (var type in typeSet) {
            foreach (var intf in type.Interfaces) {
                if (intf.ToString().Equals(baseType.ToString())) {
                    derivedTypes.Add(type);
                }
            }
        }
        return derivedTypes;
    }

    public string RemoveTypeArguments(string type) {
        var openAngleIndex = type.IndexOf('<');
        var closeAngleIndex = type.LastIndexOf('>');
        if (openAngleIndex == -1 || closeAngleIndex == -1 )
            return type;
        return type.Remove(openAngleIndex, closeAngleIndex - openAngleIndex + 1);
    }

    public bool IsInOracleExpression(SyntaxNode node) {
        foreach(var a in node.Ancestors()) {
            if (a is IfStatementSyntax)
                return true;
        }
        return false;
    }

    public bool IsReturnVarAssignment(SyntaxNode node) {
        foreach (var a in node.Ancestors()) {
            if (a is AssignmentExpressionSyntax ae) {
                if (ae.Left.ToString().Contains("r") ||
                    (ae.Left.ToString().Contains("out")))
                    return true;
                else return false;
            }
            else if (a is ExpressionStatementSyntax) {
                return true;
            } 
        }
        return true;
    }

    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var callExpr = node.Expression;

        // check to make sure in unit test namespace
        if (CheckIfInUnitTestsDecl(node) && IsReturnVarAssignment(node) && 
            !IsInOracleExpression(node) && callExpr is MemberAccessExpressionSyntax ma) {
            var Lhs = ma.Expression;
            if (Lhs is MemberAccessExpressionSyntax l) {
                var modifiedType = RemoveTypeArguments(l.ToString());
                var modifiedCall = RemoveTypeArguments(ma.Name.ToString());
                TestedMethodNames.Add(modifiedType + "::" + modifiedCall);
            } else if (Lhs is ParenthesizedExpressionSyntax p) {
                var simpleName = PullSimpleNameFromParenExpr(p);
                if (simpleName != null) {
                    var type = model.GetTypeInfo(simpleName).Type;
                    if (type is INamedTypeSymbol && type.IsAbstract) {
                        var set = GetDerivedTypes(type.OriginalDefinition);
                        foreach (var derivedType in set) {
                            var result = comp.ClassifyConversion(type, derivedType);
                            if (!result.IsThrow && result.IsExplicit) {
                                var modifiedType = RemoveTypeArguments(derivedType.ToString());
                                var modifiedCall = RemoveTypeArguments(ma.Name.ToString());
                                TestedMethodNames.Add(modifiedType + "::" + modifiedCall);
                            }
                        }                        
                    }
                }
            }
        }
        //base.VisitInvocationExpression(node);
    }
}