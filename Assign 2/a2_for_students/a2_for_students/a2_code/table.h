/****************************************************
 * Filename: table.h
 * Transition Table and function declarations necessary
	for the scanner implementation
 * as required for CST8152 - Assignment #2.
 * Version: 1.21
 * Date: 24 Sep 2020
 * Created by: Svillen Ranev - Updated by: Paulo Sousa
 * 2DO: The file is incomplete. You are to complete it.
 ***************************************************
 * TODO_01: REPLACE THIS HEADER WITH YOUR HEADER
 ***************************************************
 */

// TODO_101: Adjust the file header   

/* Macro section */
#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*  Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */
  
/* TODO_102: EOF definitions */
#define CHARSEOF0 '\0'
#define CHARSEOF255 255

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', ## ,
 *  .AND., .OR. , .NOT., SEOF,
 */
 
/* TODO_103: Error states and illegal state */
#define ES  11		/* Error state  with no retract */
#define ER  12		/* Error state  with retract */
#define IS  -1		/* Illegal state ??????????????????????????????????? */

#define MAX_INT_P 32767

 /* State transition table definition */
#define TABLE_COLUMNS 8

/* Transition table - type of states defined in separate table */
/* TODO_104: Define Transition table missing values */
int  transitionTable[][TABLE_COLUMNS] = {
	/*				[A-z],		[0-9],		.,			$,			",			SEOF,		other	*/
	/*				L(0),		D(1),		P(2),		S(3),		Q(4),		E(5),		O(6)	*/
	/* State 00 */  {1,			5,			ES,			3,			9,			ES,			ES},
	/* State 01 */  {1,			1,			2,			2,			2,			2,			2},
	/* State 02 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 03 */  {3,			3,			ES,			4,			ES,			ES,			ES},
	/* State 04 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 05 */  {ES,		5,			7,			ES,			ES,			ER,			6},
	/* State 06 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 07 */  {ES,		7,			8,			ES,			ES,			ER,			8},
	/* State 08 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 09 */  {9,			9,			9,			9,			10,			ER,			9},
	/* State 10 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 11 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS},
	/* State 12 */  {IS,		IS,			IS,			IS,			IS,			IS,			IS}
};

/* Accepting state table definition */
/* TODO_105: Define accepting states types */
#define ASWR UNKNOWN	/* accepting state with retract */
#define ASNR UNKNOWN	/* accepting state with no retract */
#define NOAS UNKNOWN	/* not accepting state */

/* List of acceptable states */
/* TODO_106: Define list of acceptable states */
int stateType[] = {
	NOAS, 		/* 00 */
	NOAS, 		/* 01 */
	UNKNOWN, 	/* 02 */
	NOAS, 		/* 03 */
	UNKNOWN, 	/* 04 */
	NOAS, 		/* 05 */
	UNKNOWN, 	/* 06 */
	NOAS, 		/* 07 */
	UNKNOWN, 	/* 08 */
	NOAS, 		/* 09 */
	UNKNOWN, 	/* 10 */
	UNKNOWN, 	/* 11 */
	UNKNOWN  	/* 12 */
};

/* TODO_107: Declare accepting states functions */
/* Accepting action function declarations */

/* Defining a new type: pointer to function (of one char * argument)
   returning Token
*/
typedef Token(*PTR_ACCFUN)(char* lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*finalStateTable[])(char lexeme[]) = {
 */
/* TODO_108: Define final state table */
PTR_ACCFUN finalStateTable[] = {
	UNKNOWN,		/* 00 */
	UNKNOWN,		/* 01 */
	UNKNOWN,		/* 02 */
	UNKNOWN,		/* 03 */
	UNKNOWN,		/* 04 */
	UNKNOWN,		/* 05 */
	UNKNOWN,		/* 06 */
	UNKNOWN,		/* 07 */
	UNKNOWN,		/* 08 */
	UNKNOWN,		/* 09 */
	UNKNOWN,		/* 10 */
	UNKNOWN,		/* 11 */
	UNKNOWN			/* 12 */
};

/* TODO_09: Define the number of Keywords from the language */
/* Keyword lookup table (.AND., .OR. and .NOT. are not keywords) */
#define KWT_SIZE UNKNOWN

/* Keyword list */
/* TODO_10: Define the list of keywords */
char* keywordTable[] = {
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
	UNKNOWN,
};

#endif
