// Generated from BKOOL.g4 by ANTLR 4.5

	package bkool.parser;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link BKOOLParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface BKOOLVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(BKOOLParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#class_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_decl(BKOOLParser.Class_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#list_members}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList_members(BKOOLParser.List_membersContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#constuctor_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstuctor_decl(BKOOLParser.Constuctor_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#attr_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttr_decl(BKOOLParser.Attr_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#method_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_decl(BKOOLParser.Method_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#list_params}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList_params(BKOOLParser.List_paramsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#const_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConst_decl(BKOOLParser.Const_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#const_decl_rhs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConst_decl_rhs(BKOOLParser.Const_decl_rhsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#var_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar_decl(BKOOLParser.Var_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#var_list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar_list(BKOOLParser.Var_listContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#id_list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitId_list(BKOOLParser.Id_listContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#bkool_type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBkool_type(BKOOLParser.Bkool_typeContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#primitive_type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimitive_type(BKOOLParser.Primitive_typeContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#class_type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClass_type(BKOOLParser.Class_typeContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#array_type}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArray_type(BKOOLParser.Array_typeContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#block_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock_statement(BKOOLParser.Block_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#if_then_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIf_then_statement(BKOOLParser.If_then_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#then_else_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitThen_else_statement(BKOOLParser.Then_else_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#while_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhile_statement(BKOOLParser.While_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#do_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDo_statement(BKOOLParser.Do_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#method_call_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_call_stmt(BKOOLParser.Method_call_stmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#single_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingle_statement(BKOOLParser.Single_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_statement(BKOOLParser.Io_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#assign_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssign_statement(BKOOLParser.Assign_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#assign_lhs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssign_lhs(BKOOLParser.Assign_lhsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#assign_rhs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssign_rhs(BKOOLParser.Assign_rhsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#break_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBreak_statement(BKOOLParser.Break_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#continue_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContinue_statement(BKOOLParser.Continue_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#return_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturn_statement(BKOOLParser.Return_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#obj_creation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitObj_creation(BKOOLParser.Obj_creationContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#obj_params}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitObj_params(BKOOLParser.Obj_paramsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#list_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList_expr(BKOOLParser.List_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#index_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndex_expr(BKOOLParser.Index_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#name_index_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitName_index_expr(BKOOLParser.Name_index_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndex(BKOOLParser.IndexContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#int_const_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt_const_expr(BKOOLParser.Int_const_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#float_const_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat_const_expr(BKOOLParser.Float_const_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#string_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitString_expr(BKOOLParser.String_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#string_const_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitString_const_expr(BKOOLParser.String_const_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#boolean_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolean_expr(BKOOLParser.Boolean_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#comparison_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison_expr(BKOOLParser.Comparison_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#comparison_operand}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison_operand(BKOOLParser.Comparison_operandContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#comp_operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComp_operator(BKOOLParser.Comp_operatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#arithmetic_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmetic_expr(BKOOLParser.Arithmetic_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#arithmetic_operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmetic_operator(BKOOLParser.Arithmetic_operatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#int_arithmetic_operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInt_arithmetic_operator(BKOOLParser.Int_arithmetic_operatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#float_arithmetic_operator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloat_arithmetic_operator(BKOOLParser.Float_arithmetic_operatorContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#attr_access}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttr_access(BKOOLParser.Attr_accessContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#method_access}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_access(BKOOLParser.Method_accessContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#other_method_access}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOther_method_access(BKOOLParser.Other_method_accessContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#method_params}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod_params(BKOOLParser.Method_paramsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#list_method_params}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList_method_params(BKOOLParser.List_method_paramsContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#logical_literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogical_literal(BKOOLParser.Logical_literalContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#numeric_literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumeric_literal(BKOOLParser.Numeric_literalContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_rd_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_rd_stmt(BKOOLParser.Io_rd_stmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_wr_int_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_wr_int_stmt(BKOOLParser.Io_wr_int_stmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_wr_float_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_wr_float_stmt(BKOOLParser.Io_wr_float_stmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_wr_bool_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_wr_bool_stmt(BKOOLParser.Io_wr_bool_stmtContext ctx);
	/**
	 * Visit a parse tree produced by {@link BKOOLParser#io_wr_str_stmt}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo_wr_str_stmt(BKOOLParser.Io_wr_str_stmtContext ctx);
}