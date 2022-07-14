using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Text.Json.Serialization;

namespace TestedMethodLister;

public static class TestedMethodLister {



    public static void Main(string[] args) {
        var allTests = CSharpSyntaxTree.ParseText(File.ReadAllText(args[0]));
        CompilationUnitSyntax root = allTests.GetCompilationUnitRoot();
        root = root.NormalizeWhitespace();
        // var invocations = root.DescendantNodes()
        //                                         .OfType<InvocationExpressionSyntax>();
        TestedMethodWalker walker = new TestedMethodWalker();
        walker.Visit(root);

        foreach (var method in walker.TestedMethodNames)
            System.Console.WriteLine(method);
    }
}

class TestedMethodWalker : CSharpSyntaxWalker {

    public Dictionary<string, HashSet<string>> RootDeclToFullyQualifiedClassNameSet { get; }
    public Dictionary<string, HashSet<string>> FullyQualifiedClassNameToMethodNameSet { get; }
    public HashSet<string> TestedMethodNames { get; }
    private ClassDeclarationSyntax? currentClass;

    public TestedMethodWalker() {
        TestedMethodNames = new();
        FullyQualifiedClassNameToMethodNameSet = new();
        RootDeclToFullyQualifiedClassNameSet = new();
    }

    public string GetNonUnitTestEquivalent(string cls) {
        const string phrase = "UnitTests";
        var index = cls.IndexOf("UnitTests");
        if (index == -1) {
            return cls;
        }
        return cls.Substring(0, index) + cls.Substring(index + phrase.Count());
    }

    public (string, string) GetFullyQualifiedClassNameAndRootDecl() {
        if (currentClass == null) 
            return ("", "");
        var str = currentClass.Identifier.Text;
        var rootDecl = "";
        foreach (var node in currentClass.Ancestors()) {
            if (node is NamespaceDeclarationSyntax ns) {
                if (ns.Name is IdentifierNameSyntax ident) {
                    rootDecl = ident.GetText().ToString().TrimEnd( '\r', '\n' );
                    str = rootDecl + "." + str;
                }
            } else if (node is ClassDeclarationSyntax cls) {
                str = cls.GetText().ToString().TrimEnd( '\r', '\n' ) + "." + str;
            }
        }        
        return (str, rootDecl);
    }

    public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var (fullQualified, _) = GetFullyQualifiedClassNameAndRootDecl();
        if (!FullyQualifiedClassNameToMethodNameSet.ContainsKey(fullQualified)) {
            FullyQualifiedClassNameToMethodNameSet[fullQualified] = new HashSet<string>();
        }
        FullyQualifiedClassNameToMethodNameSet[fullQualified].Add(node.Identifier.ToString());
        base.VisitMethodDeclaration(node);
    }

    public override void VisitClassDeclaration(ClassDeclarationSyntax node)
    {
        currentClass = node;
        var (fullQualified, rootDecl) = GetFullyQualifiedClassNameAndRootDecl();
        if (!RootDeclToFullyQualifiedClassNameSet.ContainsKey(rootDecl)) {
            RootDeclToFullyQualifiedClassNameSet[rootDecl] = new HashSet<string>();
        }
        RootDeclToFullyQualifiedClassNameSet[rootDecl].Add(fullQualified);
        base.VisitClassDeclaration(node);
    }

    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var (_, rootDecl) = GetFullyQualifiedClassNameAndRootDecl();
        if (!rootDecl.Contains("UnitTests")) {
            return;
        }

        var expr = node.Expression;
        if (expr is MemberAccessExpressionSyntax id) {
            var rootDeclNonTest = GetNonUnitTestEquivalent(rootDecl);
            var methodBeingCalled = id.Name.Identifier.ToString();

            var fullQualifiedClassSet = RootDeclToFullyQualifiedClassNameSet[rootDeclNonTest];
            foreach (var fullQualified in fullQualifiedClassSet) {
                if (FullyQualifiedClassNameToMethodNameSet[fullQualified].Contains(methodBeingCalled)) {
                    var totalStr = fullQualified + "::" + methodBeingCalled;
                    TestedMethodNames.Add(totalStr);
                }
            }
        }
        base.VisitInvocationExpression(node);
    }
}