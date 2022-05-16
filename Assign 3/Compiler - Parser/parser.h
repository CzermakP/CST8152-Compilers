/**********************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
**********************************************************************
* File name:	parser.h
* Compiler:		MS Visual Studio 2019
* Author:		Patrick Czermak, Iman Dirie
* Course:		CST 8152 – Compilers, Lab Section: [ 013 ]
* Assignment:	A3.
* Date:			Apr 17 2021
* Purpose:		file holds Global variables, constants, and function 
				definitions used in parser.c 
*********************************************************************/

/* TODO_101: Adjust the function header */

/* Inclusion section */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "token.h"
#include "buffer.h"

/* Global vars */
static Token lookahead; //TODO_01
int syntaxErrorNumber = 0; //TODO_02
extern bStructure* stringLiteralTable;
extern int line;
extern Token tokenizer();
extern char* keywordTable[];

/* Mock code: remove this UNKNOWN when variables defined */
//#define UNKNOWN 0

/* TODO_102: Create ALL constants for keywords (sequence given in table.h) */
/* Constants TODO_03 */
#define	NO_ATTR	-1
#define MAIN 	0
#define IF		1
#define THEN	2
#define	ELSE	3
#define WHILE	4
#define DO		5
#define READ	6
#define WRITE	7
#define TRUE	8
#define FALSE	9
// Continue the code

/* Function definitions */
void startParser(void);
void matchToken(int, int);
void syncErrorHandler(int);
void printError();
//void printMessage(char*);

/* TODO_103: Place ALL non-terminal function declarations TODO_04 */
void program(void);
void optionalStatements(void);
void statements(void);
void statementsPrime(void);
void statement(void);
void assignmentStatement(void);
void assignmentExpression(void);
void selectionStatement(void);
void iterationStatement(void);
void preCondition(void);
void inputStatement(void);
void variableList(void);
void variableListPrime(void);
void variableIdentifier(void);
void outputStatement(void);
void outputStatementPrime(void);
void optVariableList(void);
void arithmeticExpression(void);
void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void additiveArithmeticExpressionPrime(void);
void multiplicativeArithmeticExpression(void);
void multiplicativeArithmeticExpressionPrime(void);
void primaryArithmeticExpression(void);
void stringExpression(void);
void stringExpressoinPrime(void);
void primaryStringExpression(void);
void conditionalExpression(void);
void logicalOrExpression(void);
void logicalOrExpressionPrime(void);
void logicalAndExpression(void);
void logicalAndExpressionPrime(void);
void logicalNotExpression(void);
void relationalExpression(void);
void relationalA_Expression(void);
void relationalA_ExpressionPrime(void);
void primaryA_relationalExpression(void);
void relationalS_Expression(void);
void relationalS_ExpressionPrime(void);
void primaryS_relationalExpression(void);
