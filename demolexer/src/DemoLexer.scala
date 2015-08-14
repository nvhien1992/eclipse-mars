/*
 * @author nhphung
 */

import org.antlr.v4.runtime.ANTLRFileStream
import org.antlr.v4.runtime.Token

object DemoLexer {
  def main(args: Array[String]): Unit = {
    val file = if (args.length > 0) args(0) else "test.txt";

    val lexer = new Exp(new ANTLRFileStream(file));

    //printLexeme(lexer)

    //lexer.reset()

    //printToken(lexer)

    printAll(lexer)

  }
  def printAtt(lexer: Exp, prn: Token => String): Unit = {
    val tok = lexer.nextToken()
    if (tok.getType() != Token.EOF) {
      print(prn(tok))
      printAtt(lexer, prn)
    } else println
  }

  def printLexeme(lexer: Exp) = printAtt(lexer, _.getText() + " ")

  def printToken(lexer: Exp) = printAtt(lexer, x => Exp.ruleNames(x.getType() - 1) + " ")

  def printAll(lexer: Exp) = printAtt(lexer, x => x.getText() + "\t" + Exp.ruleNames(x.getType() - 1) + "\n")

}