package truediff.compat.antlr

import org.antlr.v4.runtime.RuleContext
import org.antlr.v4.runtime.tree.RuleNode

import scala.collection.mutable

class RuleContextMapper(rulenames: Array[String]) {
  private val mapper: mutable.Map[RuleContext, DiffableRuleContext] = mutable.Map()

  def diffable(ruleNode: RuleNode): DiffableRuleContext = diffable(ruleNode.getRuleContext)

  def diffable(ctx: RuleContext): DiffableRuleContext = mapper.get(ctx) match {
    case Some(diff) => diff
    case None =>
      val diff = new DiffableRuleContext(rulenames(ctx.getRuleIndex), ctx, this)
      mapper += ctx -> diff
      diff
  }
}
