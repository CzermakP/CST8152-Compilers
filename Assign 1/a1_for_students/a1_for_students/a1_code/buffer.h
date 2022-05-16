/*************************************************************
* REPLACE the file header below with your file header (see CST8152_ASSAMG.pdf for details).
* File Name: buffer.h
* Version: 1.20.1
* Author: Svillen Ranev, Paulo Sousa, Abdulah
* Date: 1 January 2021
* Preprocessor directives, type declarations and prototypes necessary for buffer implementation
* as required for CST8152-Assignment #1.
* The file is not completed.
* You must add your function declarations (prototypes).
* You must also add your constant definitions and macros, if any.
*/

// TODO101: Adjust file header

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)			//done TODO102
#define RT_FAIL_2 UNKNOWN			// TODO103
#define LOAD_FAIL UNKNOWN			// TODO104

#define DEFAULT_SIZE 200        //done - default for initial buffer size TODO105
#define DEFAULT_INCREMENT (15)   //done - default increment factor TODO106

/* You should add your own constant definitions here */
#define MAX_SIZE SHRT_MAX-1   /* maximum capacity*/ 

/* Buffer Modes */
#define FIXMODE (0)         //done -  indicates that the buffer operates in “fixed-size” mode. TODO107
#define ADDMODE (1)         //done - indicates “additive self-incrementing” mode. TODO108
#define MULMODE (-1)         //done -  indicates “multiplicative self-incrementing” mode. TODO109

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS 0x3FFF 	// 0011.1111 1111.1111
#define SET_EOB 0x0001         //done 0000.0000.0000.0001 TODO110
#define RESET_EOB 0x000E       //done 1111.1111.1111.1110 TODO111
#define CHECK_EOB 0x0001       //done 0000.0000.0000.0001 TODO112 
#define SET_R_FLAG UNKNOWN      // TODO113
#define RESET_R_FLAG UNKNOWN    // TODO114
#define CHECK_R_FLAG UNKNOWN    // TODO115

/* Constants used in buffer */
#define MAXINCREMENT UNKNOWN 	// TODO116
#define MAX_VALUE UNKNOWN 		// TODO117
#define RT_INC_FAIL UNKNOWN		// TODO118

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
// TODO119

#endif
