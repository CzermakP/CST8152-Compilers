/************************************************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************************************************
* File name:	parser.c
* Compiler:		MS Visual Studio 2019
* Author:		Patrick Czermak, Iman Dirie
* Course:		CST 8152 – Compilers, Lab Section: [ 013 ]
* Assignment:	A3.
* Date:			Apr 17 2021
* Purpose:		This file is the main header for Parser. Purpose is to parse the incoming string
*				by matching terminal tokens. 
* Function list: startParser, matchToken, syncErrorHandler, PrintError
*				program, optionalStatements, statements, statementsPrime, statement,
*				assignmentStatement, assignmentExpression, selectionStatement, iterationStatement,
*				preCondition, inputStatement, variableList, variableListPrime, variableIdentifier, 
*				outputStatement, outputStatementPrime, optVariableList, arithmeticExpression, 
*				unaryArithmeticExpression, additiveArithmeticExpression,
*				additiveArithmeticExpressionPrime, multiplicativeArithmeticExpression,
*				multiplicativeArithmeticExpressionPrime, primaryArithmeticExpression,
*				stringExpression, stringExpressoinPrime, primaryStringExpression,
*				conditionalExpression, logicalOrExpression, logicalOrExpressionPrime,
*				logicalAndExpression, logicalAndExpressionPrime, logicalNotExpression,
*				relationalExpression, relationalA_Expression, relationalA_ExpressionPrime,
*				primaryA_relationalExpression, relationalS_Expression,
*				relationalS_ExpressionPrime, primaryS_relationalExpression
**************************************************************************************************/
/* TODO_201: Adjust the function header */

#include "parser.h"

/*************************************************************
 * Process Parser
 ************************************************************/
void startParser(void) {
	lookahead = tokenizer();
	program();
	matchToken(SEOF_T, NO_ATTR);
	printf("%s\n", "PLATY: Source file parsed");
}

/*************************************************************
 * Match Token
 ************************************************************/
/* TODO_202: Continue the development */
void matchToken(int tokenCode, int tokenAttribute) {
	int matchFlag = 1;
	switch (lookahead.code) {
	case KW_T:
	case REL_OP_T:
	case ART_OP_T:
	case LOG_OP_T:
		// TODO_05
		if (tokenAttribute != lookahead.attribute.codeType) {
			matchFlag = 0;
		}
		else if (tokenCode != lookahead.code) {
			matchFlag = 0;
		}
		break;
	default:
		//TODO_06
		if (tokenCode != lookahead.code) {
			matchFlag = 0;
		}
	}
	if (matchFlag && lookahead.code == SEOF_T)
		//TODO_07
		return;
	if (matchFlag) {
		//TODO_08
		lookahead = tokenizer();
		if (lookahead.code == ERR_T) {
			printError();
			lookahead = tokenizer();
			syntaxErrorNumber++;
			return;
		}
	}
	else {
		//TODO_09
		syncErrorHandler(tokenCode);
	}
}

/*************************************************************
 * Syncronize Error Handler
 ************************************************************/
/* TODO_203: Continue the development */
void syncErrorHandler(int syncTokenCode) {
	//TODO_10
	printError();
	syntaxErrorNumber++;

	while (lookahead.code != syncTokenCode) {
		//TODO_11
		if (lookahead.code == SEOF_T) {
			exit(syntaxErrorNumber);
			return;
		}
		lookahead = tokenizer();
	}
	if (lookahead.code != SEOF_T) {
		//TODO_12
		lookahead = tokenizer();
		return;
	}
}


/*************************************************************
 * printError
 ************************************************************/
/* TODO_204 */
void printError() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
		//TODO_13
	case ERR_T:     /* ERR_T   -1   Error Token */
		printf("%s\n", t.attribute.errLexeme);
		break;
	case SEOF_T:    /* SEOF_T   0   Source end of file token */
		printf("SEOF_T	\n");
		break;
	case AVID_T:	/* AVID_T   1   Arithmetic variable identifier token */
	case SVID_T:	/* SVID_T   2   String variable identifier token */
		printf("%s\n", t.attribute.vidLexeme);
		break;
	case FPL_T:		/* FPL_T    3   Floating point literal token */
		printf("%5.1f\n", t.attribute.floatValue);
		break;
	case INL_T:		/* INL_T    4   Integer literal token */
		printf("%d\n", t.attribute.codeType);
		break;
	case STR_T:		/* STR_T    5   String literal token */
		printf("%s\n", bGetContent(stringLiteralTable, t.attribute.contentString));
		break;
	case SCC_OP_T:	/* SCC_OP_T 6   String concatentaion operator token */
		printf("NA\n"); 
		break;
	case ASS_OP_T:	/* ASS_OP_T 7   Assignment operator token */
		printf("NA\n"); 
		break;
	case ART_OP_T:	/* ART_OP_T 8   Arithmetic operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case REL_OP_T:	/* REL_OP_T 9   Relational operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case LOG_OP_T:	/* LOG_OP_T 10  Logical operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case LPR_T:		/* LPR_T    11  Left parenthesis token */
		printf("NA\n");
		break;
	case RPR_T:		/* RPR_T    12  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T:		/* LBR_T    13  Left bracket/brace token */
		printf("NA\n");
		break;
	case RBR_T:		/* RBR_T    14  Right bracket/brace token */
		printf("NA\n");
		break;
	case KW_T:		/* KW_T     15  Keyword token */
		printf("%s\n", keywordTable[t.attribute.codeType]);
		break;
	case COM_T:		/* COM_T    16  Comma token */
		printf("NA\n");
		break;
	case EOS_T:		/* EOS_T    17  End of statement (semi-colon) token */
		printf("NA\n");
		break;
	case RTE_T:		/* RTE_T    18  Run time error token */
		printf("RTE_T");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}


/*************************************************************
 * program
 * BNF: <program> -> PLATYPUS { <opt_statements> }
 * FIRST(<program>)= {KW_T (MAIN)}.
 ************************************************************/
void program(void) {
	matchToken(KW_T, MAIN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	printf("%s\n", "PLATY: Program parsed");
}

/*************************************************************************************************
 * optionalStatements
 * BNF: <opt_statements> -> <statements> | ϵ
 * FIRST(<opt_statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ϵ }
 *************************************************************************************************/
void optionalStatements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			break;
		}
	}
}

/******************************************************************************************
 * statements
 * BNF: <statements> -> <statement><statementsPrime>
 * FIRST(<statements>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ******************************************************************************************/
void statements(void) {
	statement();
	statementsPrime();
	printf("%s\n", "PLATY: Statements parsed");
}

/**************************************************************************************************
 * statementsPrime
 * BNF: <statementsPrime> -> <statement><statementsPrime> | ϵ 
 * FIRST(<statementsPrime>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ϵ }
 **************************************************************************************************/
void statementsPrime(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement();
		statementsPrime();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF ||
			lookahead.attribute.codeType == WHILE ||
			lookahead.attribute.codeType == READ ||
			lookahead.attribute.codeType == WRITE) {
			statement();
			statementsPrime();
		}
		break;
	}
}

/***********************************************************************************************
 * statement
 * BNF: <statement> -> <assignment statement> | <selection statement> | <iteration statement> | 
						<input statement> | <output statement>
 * FIRST(<statement>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
 ***********************************************************************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignmentStatement();
		break;

	case KW_T:
		switch (lookahead.attribute.codeType) {
		case IF:
			selectionStatement();
			break;
		case WHILE:
			iterationStatement();
			break;
		case READ:
			inputStatement();
			break;
		case WRITE:
			outputStatement();
			break;
		}
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Statement parsed");
}

/*************************************************************
 * assignmentStatement
 * BNF: <assignment statement> -> <assignment expression>
 * FIRST(<assignment statement>) = { AVID_T, SVID_T }
 ************************************************************/
void assignmentStatement(void) {
	assignmentExpression();
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Assignment statement parsed");
}

/**********************************************************************************************
 * assignmentExpression
 * BNF: <assignment expression> ->  AVID = <arithmetic expression> | SVID = <string expression>
 * FIRST(<assignment expression>)  = { AVID_T, SVID_T }
 **********************************************************************************************/
void assignmentExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
		matchToken(AVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		arithmeticExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		stringExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	default:
		printError();
		break;
	}
}

/******************************************************************************
 * selectionStatement
 * BNF: <selection statement> -> IF <pre-condition>  (<conditional expression>)
 *									THEN { <opt_statements> }
 *  								ELSE { <opt_statements> } 
*
 * FIRST(<selection statement>)  = { KW_T(IF) }
 ******************************************************************************/
void selectionStatement(void) {
	matchToken(KW_T, IF);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, THEN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);

	matchToken(KW_T, ELSE);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Selection statement parsed");
}

/********************************************************************************
 * iterationStatement
 * BNF: <iteration statement> -> WHILE <pre-condition> (<conditional expression>)
 *									DO { <statements>};
 *
 * FIRST(<iteration statement>) = { KW_T(WHILE) }
 ********************************************************************************/
void iterationStatement(void) {
	matchToken(KW_T, WHILE);
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, DO);
	matchToken(LBR_T, NO_ATTR);
	statements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Iteration statement parsed");
}

/*************************************************************
 * preCondition
 * BNF: <pre-condition> -> TRUE | FALSE
 * FIRST(<pre-condition>) = { KW_T(TRUE), KW_T(FALSE) }
 ************************************************************/
void preCondition(void) {
	switch (lookahead.code) {
	case KW_T:
		switch (lookahead.attribute.codeType) {
		case TRUE:
		case FALSE:
			matchToken(KW_T, lookahead.attribute.codeType);
			printf("%s\n", "PLATY: Pre-condition parsed");
			break;
		default:
			printError();
			break;
		}
	default:
		printError();
		break;
	}
}

/* Example 2 */
/*************************************************************
 * inputStatement
 * BNF: <input statement> -> READ (<variable list>)
 * FIRST(<input statement>) = { KW_T(READ) }
 ************************************************************/
void inputStatement(void) {
	matchToken(KW_T, READ);
	matchToken(LPR_T, NO_ATTR);
	variableList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Input statement parsed");
}

/********************************************************************* 
 * variableList
 * BNF: <variable list> -> <variable identifier><variable list Prime>
 * FIRST(<variable list>) = { AVID_T, SVID_T }
 ********************************************************************/
void variableList(void) {
	variableIdentifier();
	variableListPrime();
	printf("%s\n", "PLATY: Variable list parsed");
}

/********************************************************************************
 * variableListPrime
 * BNF: <variable list Prime> -> ,<variable identifier><variable list Prime> | ϵ
 * FIRST(<variable list Prime>) = { COM_T, ϵ }
 ********************************************************************************/
void variableListPrime(void) {
	switch (lookahead.code) {
	case COM_T:
		matchToken(COM_T, NO_ATTR);
		variableIdentifier();
		variableListPrime();
		break;
	}
}

/*****************************************************
 * variableIdentifier
 * BNF: <variable identifier> -> AVID_T | SVID_T 
 * FIRST(<variable identifier>) = { AVID_T, SVID_T }
 *****************************************************/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		matchToken(lookahead.code, NO_ATTR);
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Variable identifier parsed");
}

/*************************************************************
 * outputStatement
 * BNF: <output statement> -> WRITE (<output statementPrime>)
 * FIRST(<output statement>) = { KW_T(WRITE) }
 ************************************************************/
void outputStatement(void) {
	matchToken(KW_T, WRITE);
	matchToken(LPR_T, NO_ATTR);
	outputStatementPrime();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Output statement parsed");
}

/******************************************************************
 * outputStatementPrime
 * BNF: <output statementPrime> -> <opt_variable list> | STR_T 
 * FIRST(<output statementPrime>) = { AVID_T, SVID_T, STR_T, ϵ }
 *****************************************************************/
void outputStatementPrime(void) {
	switch (lookahead.code) {
	case STR_T:
		matchToken(STR_T, NO_ATTR);
		printf("%s\n", "PLATY: Output variable list parsed");
		break;
	default:
		optVariableList();
		break;
	}
}

/*************************************************************
 * optVariableList
 * BNF: <opt_variable list> -> <variable list> | ϵ 
 * FIRST(<opt_variable list>) = { AVID_T, SVID_T, ϵ }
 ************************************************************/
void optVariableList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variableList();
		break;
	default:
		printf("%s\n", "PLATY: Output variable list parsed");
	}
}

/****************************************************************************************************
 * arithmeticExpression
 * BNF: <arithmetic expression> ->  <unary arithmetic expression> | <additive arithmetic expression>
 * FIRST(<arithmetic expression>) = { -, +, AVID_T, FPL_T, INL_T, ( }
 ***************************************************************************************************/
void arithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
		case SUB:
			unaryArithmeticExpression();
			printf("%s\n", "PLATY: Arithmetic expression parsed");
			break;
		default:
			printError();
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additiveArithmeticExpression();
		printf("%s\n", "PLATY: Arithmetic expression parsed");
		break;
	default:
		printError();
		break;
	}
}

/****************************************************************************
 * unaryArithmeticExpression
 * BNF: <unary arithmetic expression> -> - <primary arithmetic expression> | 
										 + <primary arithmetic expression>
 * FIRST(<unary arithmetic expression>) = { -, + }
 ****************************************************************************/
void unaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
		case SUB:
			matchToken(ART_OP_T, lookahead.attribute.arithmeticOperator);
			primaryArithmeticExpression();
			break;
		default:
			printError();
			break;
		}
		break;
	default:
		printError();
		break;
	}
}

/**********************************************************************************
 * additiveArithmeticExpression
 * BNF: <additive arithmetic expression> -> <multiplicative arithmetic expression>
											<additive arithmetic expression Prime>
 * FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
 **********************************************************************************/
void additiveArithmeticExpression(void) {
	multiplicativeArithmeticExpression();
	additiveArithmeticExpressionPrime();
	printf("%s\n", "PLATY: Additive arithmetic expression parsed");
}

/***********************************************************************************************************************************
 * additiveArithmeticExpressionPrime
 * BNF: <additive arithmetic expression Prime> -> + <multiplicative arithmetic expression><additive arithmetic expression Prime> |
 *												  - <multiplicative arithmetic expression><additive arithmetic expression Prime> | ϵ
 *
 * FIRST(<additive arithmetic expression Prime>) = { +, -, ϵ }
 **********************************************************************************************************************************/
void additiveArithmeticExpressionPrime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
		case SUB:
			matchToken(ART_OP_T, lookahead.attribute.arithmeticOperator);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			break;
		}
	}
}

/**********************************************************************************************
 * multiplicativeArithmeticExpression 
 * BNF: <multiplicative arithmetic expression> -> <primary arithmetic expression>
												  <multiplicative arithmetic expression Prime>
 * FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
 **********************************************************************************************/
void multiplicativeArithmeticExpression(void) {
	primaryArithmeticExpression();
	multiplicativeArithmeticExpressionPrime();
	printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
}

/****************************************************************************************************************************************
 * multiplicativeArithmeticExpressionPrime 
 * BNF: <multiplicative arithmetic expression Prime> -> *<primary arithmetic expression><multiplicative arithmetic Expression Prime> | 
 *														/<primary arithmetic expression> <multiplicative arithmetic expression Prime> | ϵ
 *
 * FIRST(<multiplicative arithmetic expression Prime>) = { *, /, ϵ } 
 ***************************************************************************************************************************************/
void multiplicativeArithmeticExpressionPrime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arithmeticOperator) {
		case MUL:
		case DIV:
			matchToken(ART_OP_T, lookahead.attribute.arithmeticOperator);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			break;
		}
	}
}

/*********************************************************************************************
 * primaryArithmeticExpression
 * BNF: <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
 * FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, ( }
 *********************************************************************************************/
void primaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Primary arithmetic expression parsed");
		break;
	case LPR_T:
		matchToken(LPR_T, NO_ATTR);
		arithmeticExpression();
		matchToken(RPR_T, NO_ATTR);
		printf("%s\n", "PLATY: Primary arithmetic expression parsed");
		break;
	default:
		printError();
		break;
	}
}

/***********************************************************************************
 * stringExpression
 * BNF: <string expression> -> <primary string expression><string expression Prime>
 * FIRST(<string expression>) = { SVID_T, STR_T }
 ***********************************************************************************/
void stringExpression(void) {
	primaryStringExpression();
	stringExpressoinPrime();
	printf("%s\n", "PLATY: String expression parsed");
}

/**********************************************************************************************
 * stringExpressionPrime
 * BNF: <string expression Prime> -> +<primary string expression><string expression Prime> | ϵ
 * FIRST(<string expression Prime>) = { +, ϵ } 
 **********************************************************************************************/
void stringExpressoinPrime(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		matchToken(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpressoinPrime();
		break;
	}
}

/*************************************************************
 * primaryStringExpression
 * BNF: <primary string expression> -> SVID_T  |  STR_T
 * FIRST(<primary string expression>) = { SVID_T, STR_T }
 ************************************************************/
void primaryStringExpression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Primary string expression parsed");
		break;
	default:
		printError();
		break;
	}
}

/***************************************************************************
 * conditionalExpression
 * BNF: <conditional expression> -> <logical OR expression>
 * FIRST(<conditional expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 ***************************************************************************/
void conditionalExpression(void) {
	logicalOrExpression();
	printf("%s\n", "PLATY: Conditional expression parsed");
}

/****************************************************************************************
 * logicalOrExpression 
 * BNF: <logical OR expression> -> <logical AND expression><logical OR expression Prime>
 * FIRST(<logical OR expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 ***************************************************************************************/
void logicalOrExpression(void) {
	logicalAndExpression();
	logicalOrExpressionPrime();
	printf("%s\n", "PLATY: Logical Or Expression parsed");
}

/******************************************************************************************************
 * logicalOrExpressionPrime 
 * BNF: <logical OR expression Prime> -> .OR. <logical AND expression><logical OR expression Prime> | ϵ
 * FIRST(<logical OR expression Prime>) = { .OR. , ϵ } 
 ******************************************************************************************************/
void logicalOrExpressionPrime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.logicalOperator) {
		case OR:
			matchToken(LOG_OP_T, OR);
			logicalAndExpression();
			logicalOrExpressionPrime();
			printf("%s\n", "PLATY: Logical OR expression parsed");
			break;
		}
		break;
	}
}

/******************************************************************************************
 * logicalAndExpression
 * BNF: <logical AND expression> -> <logical NOT expression><logical AND expression Prime>
 * FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 *****************************************************************************************/
void logicalAndExpression(void) {
	logicalNotExpression();
	logicalAndExpressionPrime();
	printf("%s\n", "PLATY: Logical And Expression parsed");
}

/*********************************************************************************************************
 * logicalAndExpressionPrime
 * BNF: <logical AND expression Prime> -> .AND. <logical NOT expression><logical AND expression Prime> | ϵ
 * FIRST(<logical AND expression Prime>) = { .AND. , ϵ } 
 ********************************************************************************************************/
void logicalAndExpressionPrime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.logicalOperator) {
		case AND:
			matchToken(LOG_OP_T, AND);
			logicalNotExpression();
			logicalAndExpressionPrime();
			printf("%s\n", "PLATY: Logical AND expression parsed");
			break;
		}
		break;
	}
}

/******************************************************************************************
 * Logical NOT Expression
 * BNF: <logical NOT expression> -> .NOT. <relational expression> | <relational expression>
 * FIRST(<logical NOT expression>) = { .NOT., AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 ******************************************************************************************/
void logicalNotExpression(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		matchToken(LOG_OP_T, NOT);
		relationalExpression();
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T:
		relationalExpression();
		break;
	default:
		printError();
		printf("%s\n", "PLATY: Relational expression parsed");
		break;
	}
	printf("%s\n", "PLATY: Logical Not Expression parsed");
}

/****************************************************************************************
 * Relational Expression
 * BNF: <relational expression> -> <relational a_expression>  | <relational s_expression> 
 * FIRST(<relational expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 *****************************************************************************************/
void relationalExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		relationalA_Expression();
		printf("%s\n", "PLATY: Relational expression parsed");
		break;
	case SVID_T:
	case STR_T:
		relationalS_Expression();
		printf("%s\n", "PLATY: Relational expression parsed");
		break;
	default:
		printError();
		break;
	}
}

/****************************************************************************************************
 * Relational a_Expression
 * BNF: <relational a_expression> -> <primary a_relational expression><relational a_expression Prime>
									 <primary a_relational expression>
 * FIRST(<relational a_expression>) = { AVID_T, FPL_T, INL_T }
 ****************************************************************************************************/
void relationalA_Expression(void) {
		primaryA_relationalExpression();
		relationalA_ExpressionPrime();
		primaryA_relationalExpression();
		printf("%s\n", "PLATY: Relational arithmetic operator parsed");
		printf("%s\n", "PLATY: Relational arithmetic expression parsed");
}

/*****************************************************************************
 * Relational a_Expression Prime
 * BNF: <relational a_expression Prime> -> < primary a_relational expression >
 * FIRST(<relational a_expression Prime>) = { ==, !=, >, < } 
 *****************************************************************************/
void relationalA_ExpressionPrime(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
		break;
	default:
		printError();
	}
}

/*********************************************************************
 * primaryA_relationalExpression
 * BNF: <primary a_relational expression> -> AVID_T  | FPL_T  | INL_T
 * FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
 *********************************************************************/
void primaryA_relationalExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Primary relational arithmetic expression parsed");
		break;
	}
}

/****************************************************************************************************
 * Relational s_Expression
 * BNF: <relational s_expression> -> <primary s_relational expression><relational s_expression Prime> 
									 <primary s_relational expression>
 * FIRST(<relational s_expression>) = { SVID_T, STR_T }
 ***************************************************************************************************/
void relationalS_Expression(void) {
	primaryS_relationalExpression();
	relationalS_ExpressionPrime();
	primaryS_relationalExpression();
	printf("%s\n", "PLATY: Relational string operator parsed");
	printf("%s\n", "PLATY: Relational string expression parsed");
}

/*****************************************************************************
 * Relational s_Expression Prime
 * BNF: <relational s_expression Prime> -> < primary s_relational expression >
 * FIRST(<relational s_expressionPrime>) = { ==, !=, >, < }
 ****************************************************************************/
void relationalS_ExpressionPrime(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
		break;
	default:
		printError();
	}
}

/***********************************************************************
 * Primary s_Relational Expression
 * BNF: <primary s_relational expression> -> <primary string expression>
 * FIRST(<primary s_relational expression>) = { SVID_T, STR_T } 
 **********************************************************************/
void primaryS_relationalExpression(void) {
	primaryStringExpression();
	printf("%s\n", "PLATY: Primary relational string expression parsed");
}
