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
public class CloneableGenerator : ISourceGenerator
{
   const string PreventDeepCopyKeyString = "PreventDeepCopy";
   const string ExplicitDeclarationKeyString = "ExplicitDeclaration";

   const string CloneableNamespace = "CloneAndCopy";
   const string CloneableAttributeString = "CloneableAttribute";
   const string CloneAttributeString = "CloneAttribute";
   const string IgnoreCloneAttributeString = "IgnoreCloneAttribute";

   const string CloneableAttributeText = """
      using System;

      namespace 
      """ + CloneableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + CloneableAttributeString + """
    : Attribute
       {
           public 
   """ + CloneableAttributeString + """
   ()
           {
           }

           public bool 
   """ + ExplicitDeclarationKeyString + """
    { get; set; }
       }
   }

   """;

   const string ClonePropertyAttributeText = """
      using System;

      namespace 
      """ + CloneableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Property, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + CloneAttributeString + """
    : Attribute
       {
           public 
   """ + CloneAttributeString + """
   ()
           {
           }

           public bool 
   """ + PreventDeepCopyKeyString + """
    { get; set; }
       }
   }

   """;

   const string IgnoreClonePropertyAttributeText = """
      using System;

      namespace 
      """ + CloneableNamespace + """

   {
       [AttributeUsage(AttributeTargets.Property, Inherited = true, AllowMultiple = false)]
       public sealed class 
   """ + IgnoreCloneAttributeString + """
    : Attribute
       {
           public 
   """ + IgnoreCloneAttributeString + """
   ()
           {
           }
       }
   }

   """;

   INamedTypeSymbol? _cloneableAttribute;
   INamedTypeSymbol? _cloneAttribute;
   INamedTypeSymbol? _ignoreCloneAttribute;

   public void Initialize(GeneratorInitializationContext context) { context.RegisterForSyntaxNotifications(() => new SyntaxReceiver()); }

   public void Execute(GeneratorExecutionContext context)
   {
      InjectCloneableAttributes(context);
      GenerateCloneMethods(context);
   }

   void GenerateCloneMethods(GeneratorExecutionContext context)
   {
      if (context.SyntaxReceiver is not SyntaxReceiver receiver) return;

      var compilation = GetCompilation(context);

      InitAttributes(compilation);

      var classSymbols = GetClassSymbols(compilation, receiver);
      foreach (var classSymbol in classSymbols)
      {
         if (!classSymbol.TryGetAttribute(_cloneableAttribute!, out var attributes)) continue;

         var attribute = attributes.Single();
         var isExplicit = (bool?)attribute.NamedArguments.FirstOrDefault(e => e.Key.Equals(ExplicitDeclarationKeyString)).Value.Value ?? false;
         context.AddSource($"{classSymbol.Name}_cloneable.cs", SourceText.From(CreateCloneableCode(classSymbol, isExplicit), Encoding.UTF8));
      }
   }

   void InitAttributes(Compilation compilation)
   {
      _cloneableAttribute = compilation.GetTypeByMetadataName($"{CloneableNamespace}.{CloneableAttributeString}")!;
      _cloneAttribute = compilation.GetTypeByMetadataName($"{CloneableNamespace}.{CloneAttributeString}")!;
      _ignoreCloneAttribute = compilation.GetTypeByMetadataName($"{CloneableNamespace}.{IgnoreCloneAttributeString}")!;
   }

   static Compilation GetCompilation(GeneratorExecutionContext context)
   {
      var options = context.Compilation.SyntaxTrees.First().Options as CSharpParseOptions;

      var compilation = context.Compilation
         .AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(CloneableAttributeText, Encoding.UTF8), options))
         .AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(ClonePropertyAttributeText, Encoding.UTF8), options))
         .AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(IgnoreClonePropertyAttributeText, Encoding.UTF8), options));
      return compilation;
   }

   string CreateCloneableCode(INamedTypeSymbol classSymbol, bool isExplicit)
   {
      var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
      var fieldAssignmentsCode = GenerateFieldAssignmentsCode(classSymbol, isExplicit);
      var assignmentsCode = fieldAssignmentsCode as (string line, bool isCloneable)[] ?? fieldAssignmentsCode.ToArray();
      var fieldAssignmentsCodeFast = assignmentsCode.Select(x =>
      {
         if (x.isCloneable) return x.line + "()";
         return x.line;
      });

      return $$"""
         using System.Collections.Generic;

         namespace {{namespaceName}}
         {
             {{GetAccessModifier(classSymbol)}} partial class {{classSymbol.Name}}
             {
                 /// <summary>
                 /// Creates a copy of {{classSymbol.Name}}.
                 /// 
                 /// <exception cref="StackOverflowException">Will occur on any object that has circular references in the hierarchy.</exception>
                 /// </summary>
                 public object Clone()
                 {
                     return new {{classSymbol.Name}}
                     {
                         {{string.Join($",{Environment.NewLine}\t\t\t\t", fieldAssignmentsCodeFast)}}
                     };
                 }
             }
         }
         """;
   }

   IEnumerable<(string line, bool isCloneable)> GenerateFieldAssignmentsCode(INamedTypeSymbol classSymbol, bool isExplicit)
   {
      var fieldNames = GetCloneableProperties(classSymbol, isExplicit);

      var fieldAssignments = fieldNames.Select(field => IsFieldCloneable(field, classSymbol)).OrderBy(x => x.isCloneable)
         .Select(x => (GenerateAssignmentCode(x.item, x.isCloneable), x.isCloneable));
      return fieldAssignments;
   }

   static string GenerateAssignmentCode(IPropertySymbol property, bool isCloneable)
   {
      return isCloneable ? $"{property.Name} = ({property.Type})this.{property.Name}.Clone" : $"{property.Name} = this.{property.Name}";
   }

   (IPropertySymbol item, bool isCloneable) IsFieldCloneable(IPropertySymbol x, INamedTypeSymbol classSymbol)
   {
      if (SymbolEqualityComparer.Default.Equals(x.Type, classSymbol) ||
         !x.Type.TryGetAttribute(_cloneableAttribute!, out var attributes)) return (x, false);

      var preventDeepCopy = (bool?)attributes.Single().NamedArguments.FirstOrDefault(e => e.Key.Equals(PreventDeepCopyKeyString)).Value.Value ??
         false;
      return (item: x, !preventDeepCopy);
   }

   static string GetAccessModifier(INamedTypeSymbol classSymbol) { return classSymbol.DeclaredAccessibility.ToString().ToLowerInvariant(); }

   IEnumerable<IPropertySymbol> GetCloneableProperties(ITypeSymbol classSymbol, bool isExplicit)
   {
      var targetSymbolMembers = classSymbol.GetMembers().OfType<IPropertySymbol>().Where(x => x.SetMethod is not null && x.CanBeReferencedByName);
      return isExplicit ? targetSymbolMembers.Where(x => x.HasAttribute(_cloneAttribute!))
         : targetSymbolMembers.Where(x => !x.HasAttribute(_ignoreCloneAttribute!));
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

   static void InjectCloneableAttributes(GeneratorExecutionContext context)
   {
      context.AddSource(CloneableAttributeString, SourceText.From(CloneableAttributeText, Encoding.UTF8));
      context.AddSource(CloneAttributeString, SourceText.From(ClonePropertyAttributeText, Encoding.UTF8));
      context.AddSource(IgnoreCloneAttributeString, SourceText.From(IgnoreClonePropertyAttributeText, Encoding.UTF8));
   }
}
