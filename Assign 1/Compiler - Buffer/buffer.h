/****************************************************************************************
* File name: buffer.h
* Compiler: MS Visual Studio 2019
* Author: Patrick Czermak (Partner: Iman Dirie)
* Course: CST 8152 – Compilers, Lab Section: [013]
* Assignment: A1.
* Date: Feb 06 2021
* Professor: Paulo Sousa(lab) / Abdulah Khadri(theory)
* Purpose: This file contains all the constants, data types, and function declarations
*          needed for buffer.c and evalBuffer.c as required by Assignment #1
* Function list: none
****************************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)			/* operation failure return value 1 */
#define RT_FAIL_2 (-2)			/* operation failure return value 2 */
#define LOAD_FAIL (-2)			/* load fail return value */             

#define DEFAULT_SIZE 200        /* default for initial buffer size */
#define DEFAULT_INCREMENT 15    /* default increment factor */

#define MAX_SIZE SHRT_MAX-1   /* maximum capacity, and also used for max value*/ 
#define POSITION_RESET 0 

/* Buffer Modes */
#define FIXMODE (0)           /* indicates that the buffer operates in “fixed-size” mode */
#define FIX_MODE_CHAR 'f'
#define ADDMODE (1)           /* indicates “additive self-incrementing” mode */
#define ADD_MODE_CHAR 'a'
#define MULMODE (-1)          /* indicates “multiplicative self-incrementing” mode */
#define MUL_MODE_CHAR 'm'

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS 0x3FFF /* 0011.1111 1111.1111 */
#define SET_EOB 0x8000         /* 1000.0000.0000.0000 */ 
#define RESET_EOB 0x7FFF       /* 0111.1111.1111.1111 */ 
#define CHECK_EOB 0x8000       /* 1000.0000.0000.0000 */
#define SET_R_FLAG 0x4000      /* 0100.0000.0000.0000 */  
#define RESET_R_FLAG 0xBFFF    /* 1011.1111.1111.1111 */
#define CHECK_R_FLAG 0x4000    /* 0100.0000.0000.0000 */

/* more constant definitions used in buffer */ 
#define MAXINCREMENT 100     	/* default max increment*/
/* #define MAX_VALUE UNKNOWN      using MAX_SIZE as both constants are 'SHRT_MAX-1'. Confirmed with Paulo :) */
#define RT_INC_FAIL 0x100		  

/* user data type declarations */
typedef struct Buffer {
	char* content;         /* pointer to the beginning of character array (character buffer) */
	short size;            /* current dynamic memory size (in bytes) allocated to buffer */
	char  increment;       /* character array increment factor */
	char  mode;            /* operational mode indicator*/
	short addCOffset;      /* the offset (in chars) to the add-character location */
	short getCOffset;      /* the offset (in chars) to the get-character location */
	short markOffset;      /* the offset (in chars) to the mark location */
	unsigned short flags;  /* contains character array reallocation and end-of-buffer flag */
} bStructure, *bPointer;

/* Function declarations */
bPointer bCreate(short size, char increment, char mode);
bPointer bAddCh(bPointer const pBuffer, char ch);
int bClean(bPointer const pBuffer);
int bFree(bPointer const pBuffer);
int bIsFull(bPointer const pBuffer);
short bGetAddChOffset(bPointer const pBuffer);
short bGetSize(bPointer const pBuffer);
int bGetMode(bPointer const pBuffer);
short bGetMarkOffset(bPointer const pBuffer);
short bSetMarkOffset(bPointer const pBuffer, short mark);
bPointer bFinish(bPointer const pBuffer, char ch);
int bDisplay(bPointer const pBuffer, char nl);
int bLoad(bPointer const pBuffer, FILE* const fi);
int bIsEmpty(bPointer const pBuffer);
char bGetCh(bPointer const pBuffer);
int bRewind(bPointer const pBuffer);
bPointer bRetract(bPointer const pBuffer);
short bRestore(bPointer const pBuffer);
short bGetChOffset(bPointer const pBuffer);
size_t bGetIncrement(bPointer const pBuffer);
char* bGetContent(bPointer const pBuffer, short chPosition); 
short bufferAddCPosition(bPointer const pBuffer); 
unsigned short bGetFlags(bPointer const pBuffer);

#endif
