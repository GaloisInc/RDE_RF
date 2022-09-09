package Enricher

import Parsers.ParserTypes.ParsedDocument
import Types.DocReference.DocReference
import Types.DocumentInfos.{DocumentInfo, LandoDocumentInfo}
import Types.{DocRelation, DocRelationFromParser, ReferenceName}
import Utils.Matcher

trait Enricher {
  def enrich(doc: Array[ParsedDocument]): Array[DocumentInfo]
}

object LandoEnricher {


  def enrichRelations(relations: Set[DocRelationFromParser], references: Set[DocReference], docName: String): Set[DocRelation] = {
    val enrichedRels = relations.map(rel => enrichRelation(rel, references, docName))
    enrichedRels
  } ensuring (res => res.size == relations.size)

  private def enrichRelation(relation: DocRelationFromParser, references: Set[DocReference], docName: String): DocRelation = {
    require(references.nonEmpty, "references must not be empty")
    val sourceReference = references.filter(ref => Matcher.referenceNameMatches(relation.getSourceName, ref.getReference))
    val targetReference = references.filter(ref => Matcher.referenceNameMatches(relation.getTargetName, ref.getReference))

    assert(sourceReference.nonEmpty, s"Relation source reference not found: ${relation.getSourceName} in $docName")
    assert(targetReference.nonEmpty, s"Relation target reference not found: ${relation.getTargetName} in $docName")
    DocRelation(relation.documentName, relation.relationReference, relation.relationType, relation.originalLine, sourceReference.headOption, targetReference.headOption)
  } ensuring (res => res.documentName == relation.documentName && res.getRelationType == relation.relationType && res.originalLine == relation.originalLine)
}
