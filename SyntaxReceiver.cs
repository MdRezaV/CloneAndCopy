using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CloneAndCopy;

/// <summary>
///    Created on demand before each generation pass
/// </summary>
class SyntaxReceiver : ISyntaxReceiver
{
   public IList<ClassDeclarationSyntax> CandidateClasses { get; } = new List<ClassDeclarationSyntax>();

   /// <summary>
   ///    Called for every syntax node in the compilation, we can inspect the nodes and save any information useful for
   ///    generation
   /// </summary>
   public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
   {
      // any field with at least one attribute is a candidate for being cloneable
      if (syntaxNode is ClassDeclarationSyntax { AttributeLists.Count: > 0 } classDeclarationSyntax) CandidateClasses.Add(classDeclarationSyntax);
   }
}
