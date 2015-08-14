package bkool.checker

//import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.io.{ FileInputStream, PrintWriter }
import org.antlr.v4.runtime.{ ANTLRInputStream, CommonTokenStream, ParserRuleContext }
import org.antlr.v4.runtime.tree.{ ParseTreeWalker, TerminalNode, ErrorNode, ParseTree }
import bkool.parser._

object TestChecker {

  // please don't change Name of this class and name, parameter of this function
  def test(input: ANTLRInputStream, dest: PrintWriter) {

    val lexer = new BKOOLLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new BKOOLParser(tokens)

    val prog = parser.program
    val check = new StaticChecker
    check.visit(prog)
  }

}

class StaticChecker extends BKOOLBaseVisitor[Object] {

}

trait BKOOLType
case object IntType extends BKOOLType
case object FloatType extends BKOOLType
case object BoolType extends BKOOLType
case object StrType extends BKOOLType
case object VoidType extends BKOOLType
case object ArrayType extends BKOOLType
case object ClassType extends BKOOLType

case class MethodType(param_type: List[BKOOLType], ret_type: BKOOLType) extends BKOOLType
case class JointType(list_type: List[BKOOLType]) extends BKOOLType

class BuildDeclaration extends BKOOLBaseVisitor[Object] {

  override def visitProgram(ctx: BKOOLParser.ProgramContext) = ctx.class_decl().asScala.map(visit).toList.asInstanceOf[List[List[(String, BKOOLType)]]].flatten

  override def visitClass_decl(ctx: BKOOLParser.Class_declContext) = ctx.ID() //TODO

  override def visitVar_decl(ctx: BKOOLParser.Var_declContext) = visit(ctx.var_list())

  override def visitVar_list(ctx: BKOOLParser.Var_listContext) = {
    val id_lst = visit(ctx.id_list()).asInstanceOf[List[String]]
    val var_type = visit(ctx.bkool_type()).asInstanceOf[BKOOLType]
    id_lst.map(x => (x, var_type))
  }

  override def visitId_list(ctx: BKOOLParser.Id_listContext) = ctx.ID().asScala.map(visit).toList

  override def visitBkool_type(ctx: BKOOLParser.Bkool_typeContext) = ctx.children.asScala.map(visit).toList.asInstanceOf[List[List[(String, BKOOLType)]]].flatten //TODO

  override def visitPrimitive_type(ctx: BKOOLParser.Primitive_typeContext) = ctx.getChild(0).getText match {
    case "integer" => IntType
    case "float" => FloatType
    case "bool" => BoolType
    case "void" => VoidType
    case "string" => StrType
  }

  override def visitClass_type(ctx: BKOOLParser.Class_typeContext) = {
    val class_type = ctx.ID().getText
    (class_type, ClassType)
  }

  override def visitArray_type(ctx: BKOOLParser.Array_typeContext) = (visit(ctx.getChild(0)), visit(ctx.getChild(1)))

  override def visitMethod_decl(ctx: BKOOLParser.Method_declContext) = {
    val name = ctx.ID().getText
    val partype = if (ctx.list_params() != null) visit(ctx.list_params()).asInstanceOf[List[BKOOLType]] else List()
    val rettype = visit(ctx.bkool_type()).asInstanceOf[BKOOLType]
    List((name, MethodType(partype, rettype)))
  }

  override def visitList_params(ctx: BKOOLParser.List_paramsContext) =
    ctx.var_list()
      .asScala
      .map(visit)
      .toList
      .asInstanceOf[List[List[(String, BKOOLType)]]]
      .flatten
      .map(_ match {
        case (_, t) => t
      })
  
    override def visitTerminal(node: TerminalNode) = node.getText
  }

  //class TypeChecking(env: List[(String, MCType)]) extends MCBaseVisitor[Object] {
  //  def lookup[T](name: String, lst: List[T], func: T => String): Option[T] = lst match {
  //    case List() => None
  //    case head :: tail => if (func(head) == name) Some(head) else lookup(name, tail, func)
  //  }
  //  def typecheck(pt: MCType, at: MCType): Boolean = pt == at
  //  def typecheck(pt: List[MCType], at: List[MCType]): Boolean =
  //    if (pt.length != at.length) false
  //    else if (pt.length == 0) true
  //    else pt.zip(at).forall(x => typecheck(x._1, x._2))
  //  def typecheck(pt: JointType, at: List[MCType]): Boolean = typecheck(pt.typlst, at)
  //
  //  override def visitCall(ctx: MCParser.CallContext) = {
  //    val id = ctx.IDENTIFIER.getText
  //    val iddecl = lookup(id, env, (x: (String, MCType)) => x._1)
  //    if (iddecl == None)
  //      throw UndeclareIdentifier(id)
  //    else {
  //      val explst = ctx.explist()
  //      val at = if (explst != null) visit(explst).asInstanceOf[JointType].typlst else List()
  //
  //      iddecl match {
  //        case Some((_, MethodType(pt, rt))) => if (typecheck(pt, at)) rt else throw TypeMismatchInStatement(ctx.getText)
  //        case _ => throw TypeMismatchInStatement(ctx.getText)
  //      }
  //    }
  //  }
  //
  //  override def visitExplist(ctx: MCParser.ExplistContext) = JointType(ctx.exp().asScala.map(visit).toList.asInstanceOf[List[MCType]])
  //
  //  override def visitExp(ctx: MCParser.ExpContext) =
  //    ctx.moreterm().asScala.map(visit).toList.
  //      asInstanceOf[List[(String, MCType)]].
  //      foldLeft(visit(ctx.term()).asInstanceOf[MCType])((x, y) => y match {
  //        case (_, t) => if (typecheck(x, t)) t else throw TypeMismatchInExpression(ctx.getText)
  //      })
  //
  //  override def visitMoreterm(ctx: MCParser.MoretermContext) = (visit(ctx.getChild(0)), visit(ctx.getChild(1)))
  //
  //  override def visitTerm(ctx: MCParser.TermContext) =
  //    ctx.morefact().asScala.map(visit).toList.
  //      asInstanceOf[List[(String, MCType)]].
  //      foldLeft(visit(ctx.fact()).asInstanceOf[MCType])((x, y) => y match {
  //        case ("*", t) => if (typecheck(x, t)) t else throw TypeMismatchInExpression(ctx.getText)
  //        case ("/", t) => if (typecheck(x, t)) FloatType else throw TypeMismatchInExpression(ctx.getText)
  //      })
  //
  //  override def visitMorefact(ctx: MCParser.MorefactContext) = (visit(ctx.getChild(0)), visit(ctx.getChild(1)))
  //
  //  override def visitFact(ctx: MCParser.FactContext) = {
  //    if (ctx.INTLIT() != null) IntType
  //    else if (ctx.FLOATLIT() != null) FloatType
  //    else if (ctx.IDENTIFIER() != null) {
  //      val id = ctx.IDENTIFIER.getText
  //      val iddecl = lookup(id, env, (x: (String, MCType)) => x._1)
  //      if (iddecl == None)
  //        throw UndeclareIdentifier(id)
  //      else {
  //        val explst = ctx.explist()
  //        if (ctx.LBRAC() != null) {
  //          val at = if (explst != null) visit(explst).asInstanceOf[JointType].typlst else List()
  //          iddecl match {
  //            case Some((_, MethodType(pt, rt))) => if (typecheck(pt, at)) rt else throw TypeMismatchInExpression(ctx.getText)
  //            case _ => throw TypeMismatchInExpression(ctx.getText)
  //          }
  //        } else
  //          iddecl match {
  //            case Some((_, IntType)) => IntType
  //            case Some((_, FloatType)) => FloatType
  //            case _ => throw TypeMismatchInExpression(ctx.getText)
  //          }
  //      }
  //    } else
  //      visit(ctx.exp())
  //  }
  //
  //  override def visitTerminal(node: TerminalNode) = node.getText
//}

