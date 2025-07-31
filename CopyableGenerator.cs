using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace CloneAndCopy;

[Generator]
public class CopyableGenerator : ISourceGenerator
{
   const string PreventDeepCopyKeyString = "PreventDeepCopy";
   const string ExplicitDeclarationKeyString = "ExplicitDeclaration";

   const string CopyableNamespace = "CloneAndCopy";
   const string CopyableAttributeString = "CopyableAttribute";
   const string CopyAttributeString = "CopyAttribute";
   const string IgnoreCopyAttributeString = "IgnoreCopyAttribute";

   const string CopyableAttributeText = """
      using System;

      namespace 
      """ + CopyableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + CopyableAttributeString + """
    : Attribute
       {
           public 
   """ + CopyableAttributeString + """
   ()
           {
           }

           public bool 
   """ + ExplicitDeclarationKeyString + """
    { get; set; }
       }
   }

   """;

   const string CopyPropertyAttributeText = """
      using System;

      namespace 
      """ + CopyableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Property, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + CopyAttributeString + """
    : Attribute
       {
           public 
   """ + CopyAttributeString + """
   ()
           {
           }

           public bool 
   """ + PreventDeepCopyKeyString + """
    { get; set; }
       }
   }

   """;

   const string IgnoreCopyPropertyAttributeText = """
      using System;

      namespace 
      """ + CopyableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Property, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + IgnoreCopyAttributeString + """
    : Attribute
       {
           public 
   """ + IgnoreCopyAttributeString + """
   ()
           {
           }
       }
   }

   """;

   INamedTypeSymbol? _copyableAttribute;
   INamedTypeSymbol? _copyAttribute;
   INamedTypeSymbol? _ignoreCopyAttribute;

   public void Initialize(GeneratorInitializationContext context) { context.RegisterForSyntaxNotifications(() => new SyntaxReceiver()); }

   public void Execute(GeneratorExecutionContext context)
   {
      InjectCopyableAttributes(context);
      GenerateCopyMethods(context);
   }

   void GenerateCopyMethods(GeneratorExecutionContext context)
   {
      if (context.SyntaxReceiver is not SyntaxReceiver receiver) return;

      var compilation = GetCompilation(context);

      InitAttributes(compilation);

      var classSymbols = GetClassSymbols(compilation, receiver);
      foreach (var classSymbol in classSymbols)
      {
         if (!classSymbol.TryGetAttribute(_copyableAttribute!, out var attributes)) continue;

         var attribute = attributes.Single();
         var isExplicit = (bool?)attribute.NamedArguments.FirstOrDefault(e => e.Key.Equals(ExplicitDeclarationKeyString)).Value.Value ?? false;
         context.AddSource($"{classSymbol.Name}_copyable.cs", SourceText.From(CreateCopyableCode(classSymbol, isExplicit), Encoding.UTF8));
      }
   }

   void InitAttributes(Compilation compilation)
   {
      _copyableAttribute = compilation.GetTypeByMetadataName($"{CopyableNamespace}.{CopyableAttributeString}")!;
      _copyAttribute = compilation.GetTypeByMetadataName($"{CopyableNamespace}.{CopyAttributeString}")!;
      _ignoreCopyAttribute = compilation.GetTypeByMetadataName($"{CopyableNamespace}.{IgnoreCopyAttributeString}")!;
   }

   static Compilation GetCompilation(GeneratorExecutionContext context)
   {
      var options = context.Compilation.SyntaxTrees.First().Options as CSharpParseOptions;

      var compilation = context.Compilation.AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(CopyableAttributeText, Encoding.UTF8), options))
         .AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(CopyPropertyAttributeText, Encoding.UTF8), options))
         .AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(IgnoreCopyPropertyAttributeText, Encoding.UTF8), options));
      return compilation;
   }

   string CreateCopyableCode(INamedTypeSymbol classSymbol, bool isExplicit)
   {
      var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
      var fieldAssignmentsCode = GenerateFieldAssignmentsCode(classSymbol, isExplicit);

      return $$"""
         using System.Collections.Generic;

         namespace {{namespaceName}}
         {
             {{GetAccessModifier(classSymbol)}} partial class {{classSymbol.Name}}
             {
                 /// <summary>
                 /// Copy fields of other into this object.
                 /// 
                 /// <exception cref="StackOverflowException">Will occur on any object that has circular references in the hierarchy.</exception>
                 /// </summary>
                 public void CopyFrom(object other)
                 {
                    if (other is {{classSymbol.Name}} c)
                    {
                         {{string.Join($"{Environment.NewLine}\t\t\t\t", fieldAssignmentsCode)}}
                    }
                 }
             }
         }
         """;
   }

   IEnumerable<string> GenerateFieldAssignmentsCode(INamedTypeSymbol classSymbol, bool isExplicit)
   {
      var fieldNames = GetCopyableProperties(classSymbol, isExplicit);

      var fieldAssignments = fieldNames.Select(x => GenerateAssignmentCode(x.Name));
      return fieldAssignments;
   }

   static string GenerateAssignmentCode(string name) { return $"{name} = c.{name};"; }

   static string GetAccessModifier(INamedTypeSymbol classSymbol) { return classSymbol.DeclaredAccessibility.ToString().ToLowerInvariant(); }

   IEnumerable<IPropertySymbol> GetCopyableProperties(ITypeSymbol classSymbol, bool isExplicit)
   {
      var targetSymbolMembers = classSymbol.GetMembers().OfType<IPropertySymbol>().Where(x => x.SetMethod is not null && x.CanBeReferencedByName);
      return isExplicit ? targetSymbolMembers.Where(x => x.HasAttribute(_copyAttribute!))
         : targetSymbolMembers.Where(x => !x.HasAttribute(_ignoreCopyAttribute!));
   }

   static IEnumerable<INamedTypeSymbol> GetClassSymbols(Compilation compilation, SyntaxReceiver receiver)
   {
      return receiver.CandidateClasses.Select(clazz => GetClassSymbol(compilation, clazz));
   }

   static INamedTypeSymbol GetClassSymbol(Compilation compilation, ClassDeclarationSyntax clazz)
   {
      var model = compilation.GetSemanticModel(clazz.SyntaxTree);
      var classSymbol = model.GetDeclaredSymbol(clazz)!;
      return classSymbol;
   }

   static void InjectCopyableAttributes(GeneratorExecutionContext context)
   {
      context.AddSource(CopyableAttributeString, SourceText.From(CopyableAttributeText, Encoding.UTF8));
      context.AddSource(CopyAttributeString, SourceText.From(CopyPropertyAttributeText, Encoding.UTF8));
      context.AddSource(IgnoreCopyAttributeString, SourceText.From(IgnoreCopyPropertyAttributeText, Encoding.UTF8));
   }
}
