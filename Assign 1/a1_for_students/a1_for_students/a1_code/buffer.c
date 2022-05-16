/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Fall, 2020
* This Code is EXCLUSIVE for professors (must not be shared)
* Author: Svillen Ranev - Paulo Sousa - Abdulah.
*************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Paulo Sousa
* Course: CST 8152 – Compilers, Lab Section: [011, 012, 013, 014]
* Assignment: A1.
* Date: Jan 01 2021
* Professor: Paulo Sousa / Abdulah
* Purpose: This file is the main code for Buffer (A1)
* Function list: (...).
*************************************************************/
// TODO201: Adjust file header

#include "buffer.h"

/************************************************************
*  TODO202: Adjust function header
**************************************************************/
bPointer bCreate(short size, char increment, char mode) {
	bPointer b = NULL;
	// TODO203: Follow specification
	return b;
}


/************************************************************
*  TODO204: Adjust function header
**************************************************************/
bPointer bAddCh(bPointer const pBuffer, char ch) {
	// TODO205: Follow specification
	return pBuffer;
}

/************************************************************
*  TODO206: Adjust function header
**************************************************************/
int bClean(bPointer const pBuffer) {
	// TODO207: Follow specification
	return 0;
}

/************************************************************
*  TODO208: Adjust function header
**************************************************************/
int bFree(bPointer const pBuffer) {
	// TODO209: Follow specification
	return 0;
}

/************************************************************
*  TODO210: Adjust function header
**************************************************************/
int bIsFull(bPointer const pBuffer) {
	// TODO211: Follow specification
	return 0;
}

/************************************************************
*  TODO212: Adjust function header
**************************************************************/
short bGetAddChOffset(bPointer const pBuffer) {
	// TODO213: Follow specification
	return 0;
}

/************************************************************
*  TODO214: Adjust function header
**************************************************************/
short bGetSize(bPointer const pBuffer) {
	// TODO215: Follow specification
	return 0;
}

/************************************************************
*  TODO216: Adjust function header
**************************************************************/
int bGetMode(bPointer const pBuffer) {
	// TODO217: Follow specification
	return 0;
}

/************************************************************
*  TODO218: Adjust function header
**************************************************************/
short bGetMarkOffset(bPointer const pBuffer) {
	// TODO219: Follow specification
	return 0;
}

/************************************************************
*  TODO220: Adjust function header
**************************************************************/
short bSetMarkOffset(bPointer const pBuffer, short mark) {
	// TODO221: Follow specification
	return 0;
}

/************************************************************
*  TODO222: Adjust function header
**************************************************************/
bPointer bFinish(bPointer const pBuffer, char ch) {
	// TODO223: Follow specification
	return pBuffer;
}

/************************************************************
*  TODO224: Adjust function header
**************************************************************/
int bDisplay(bPointer const pBuffer, char nl) {
	// TODO225: Follow specification
	return 0;
}

/************************************************************
*  TODO226: Adjust function header
**************************************************************/
int bLoad(bPointer const pBuffer, FILE* const fi) {
	// TODO227: Follow specification
	return 0;
}

/************************************************************
*  TODO228: Adjust function header
**************************************************************/
int bIsEmpty(bPointer const pBuffer) {
	// TODO229: Follow specification
	return 0;
}


/************************************************************
*  TODO230: Adjust function header
**************************************************************/
char bGetCh(bPointer const pBuffer) {
	// TODO231: Follow specification
	return '\0';
}


/************************************************************
*  TODO232: Adjust function header
**************************************************************/
int bRewind(bPointer const pBuffer) {
	// TODO233: Follow specification
	return 0;
}


/************************************************************
*  TODO234: Adjust function header
**************************************************************/
bPointer bRetract(bPointer const pBuffer) {
	// TODO235: Follow specification
	return pBuffer;
}

/************************************************************
*  TODO236: Adjust function header
**************************************************************/
short bRestore(bPointer const pBuffer) {
	// TODO237: Follow specification
	return 0;
}

/************************************************************
*  TODO238: Adjust function header
**************************************************************/
short bGetChOffset(bPointer const pBuffer) {
	// TODO239: Follow specification
	return 0;
}


/************************************************************
*  TODO240: Adjust function header
**************************************************************/
size_t bGetIncrement(bPointer const pBuffer) {
	// TODO241: Follow specification
	return 0;
}


/************************************************************
*  TODO242: Adjust function header
**************************************************************/
char* bGetContent(bPointer const pBuffer, short chPosition) {
	// TODO243: Follow specification
	return NULL;
}


/************************************************************
*  TODO244: Adjust function header
**************************************************************/
short bufferAddCPosition(bStructure* const pBuffer) {
	// TODO245: Follow specification
	return 0;
}


/************************************************************
*  TODO246: Adjust function header
**************************************************************/
#define FLAGS_
#undef FLAGS_
#ifndef FLAGS_
unsigned short bGetFlags(bPointer const pBuffer) {
	// TODO247: Follow specification
	return 0;
}
#else
#define // TODO248: Macro definition for flags
#endif
