/********************************************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Patrick Czermak (Partner: Iman Dirie)
* Course: CST 8152 – Compilers, Lab Section: [013]
* Assignment: A1.
* Date: Feb 06 2021
* Professor: Paulo Sousa(lab) / Abdulah Khadri(theory)
* Purpose: This file is the main code, containing all the functions needed for the Buffer.
* Function list: (bCreate, bAddCh, bClean, bFree, bIsFull, bGetAddChOffset, bGetSize, 
*                 bGetMode, bGetMarkOffset, bSetMarkOffset, bFinish, bDisplay, bLoad, 
*				  bIsEmpty, bGetCh, bRewind, bRetract, bRestore, bGetChOffset, bGetIncrement, 
*				  bGetContent, bufferAddCPosition, bGetFlags)
*********************************************************************************************/

#include "buffer.h"

/*************************************************************************************************************************************************************
* Function Name: bCreate
* Purpose: To allocate memory/space for the buffer structure and it's parameters
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: calloc(), malloc(), free()
* Parameters: short size - the starting size of the buffer structure, char increment - the starting increment factor for the character array, 
*             char mode - the operational mode of the buffer structure 
* Return Value: NULL for any errors, buffer structure when successful
* Algorithm: Initialize pointer to the buffer structure, Complete error checks on the paramters being passed in, Confirm size and increment based on the modes,
*            Allocate memory/space for the buffer structure, Set defualt values for the buffer structures variables, Return the buffer structure
***************************************************************************************************************************************************************/
bPointer bCreate(short size, char increment, char mode) {

	bPointer pBuffer = NULL; /* Initialize pointer to the buffer structure */
	unsigned char tempIncrement = (unsigned char)increment;

	/* ensuring the size is within the proper range */ 
	if (size < 0 || size > MAX_SIZE) { 
		return NULL;
	}

	/* ensuring the mode is proper */
	if (mode != 'f' && mode != 'a' && mode != 'm') {
		return NULL;
	}

	/* if size is set to 0, use the DEFAULT_SIZE */
	if (size == 0) {
		size = DEFAULT_SIZE;
		/* if mode = 'a' or 'm' , set 'increment' to DEFAULT_INCREMENT = 15, else set 'increment' to 0 */
		if (mode == 'a' || mode == 'm') {
			increment = DEFAULT_INCREMENT;
			tempIncrement = (unsigned char)increment;
		}
		/* for mode = 'f' */
		else { 
			increment = 0;
			tempIncrement = (unsigned char)increment;
		}
	}

	/* setting mode and increment */
	/* if in fixed mode and increment are 0 */
	if (mode == 'f' || tempIncrement == 0) {
		mode = FIXMODE;
		increment = 0;
	}
	/* if in additive mode, and increment is between 1-255 inclusive */
	else if (mode == 'a' && tempIncrement >= 1 && tempIncrement <= 255) {
		mode = ADDMODE;
	}
	/* if in multiplicative mode, and increment is between 1-100 inclusive */
	else if (mode == 'm' && tempIncrement >= 1 && tempIncrement <= MAXINCREMENT) {
		mode = MULMODE;
	}
	else {
		return NULL;
	}
	
	/* Allocate space within memory for the buffer structure */
	pBuffer = (bPointer)calloc(1, sizeof(bStructure)); 
	/* ensure the buffer has been createedin memory */
	if (!pBuffer) { 
		return NULL;
	}

	/* allocate space within memory for content character array */
	pBuffer->content = (char*)malloc(sizeof(char) * size);  

	/* check to make sure the content character array has been created in memory, if not free space used for buffer */
	if (!pBuffer->content) {  
		free(pBuffer);
		return NULL;
	}

	/* assigning the variables to the Buffer structure */
	pBuffer->size = size;
	pBuffer->increment = increment;
	pBuffer->mode = mode;
	pBuffer->flags = DEFAULT_FLAGS;

	return pBuffer;
}

/*********************************************************************************************************************************************
* Function Name: bAddCh
* Purpose: To add a character to the buffer structures content character array
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: bIsFull(), realloc(), bGetFlags()
* Parameters: bPointer const pBuffer - pointer to the buffer structure, char ch - the character being added into the content character array
* Return Value: NULL on any errors, pBuffer when successful 
* Algorithm: Reset the buffer structures flag field, Determine which mode the buffer structure shoudl execute in, 
*            Allocate more memory/space to the content character array when full, 
*            Reset the buffer structures falg field if the memory location for the content character array has changed,
*            Add the character to the content character array, Return the buffer
*********************************************************************************************************************************************/
bPointer bAddCh(bPointer const pBuffer, char ch) {
	
	short availableSpace = 0;
	int newIncrement = 0;  
	short newSize = 0; 
	char* tempContent; 
	size_t tempIncrement = bGetIncrement(pBuffer); 

	/* if failure with buffer, return NULL*/
	if (!pBuffer) {
		return NULL;
	}
	
	/* resetting the flags field r_flag bit to 0 */
	pBuffer->flags = pBuffer->flags & RESET_R_FLAG; 

	/* if buffer is full, resize it */
	if (bIsFull(pBuffer) == 1) {

		short size = pBuffer->size;
		
		/* If the operational mode is FIXMODE, the function returns NULL */
		if (pBuffer->mode == FIXMODE) { 
			return NULL;
		}
		/* else the operational mode is ADDMODE, it attempts to increase the current size of 
		   the buffer to a new size by adding increment(converted to bytes) to size */
		else if (pBuffer->mode == ADDMODE) {
			/* if the size is full, return NULL */
			if (size == MAX_SIZE) {  
				return NULL;
			}
			newSize = size + (short)tempIncrement;
			/* check that newSize (the result from the operation is positive) */
			if (newSize < 0) {
				return NULL;
			}
			/* check newSize (does not exceed the MAXIMUM ALLOWED POSITIVE VALUE –1 aka MAX_SIZE) */
			else if (newSize > MAX_SIZE) {
				newSize = MAX_SIZE;
			}
		}
		/* else the operational mode is MULMODE, it attempts to increase the current size of the buffer to a new size */
		else if (pBuffer->mode == MULMODE) {
			/* if the size is full, return NULL */
			if (size == MAX_SIZE) { 
				return NULL;
			}
			/* attempts to increase the current size using the following formula: */
			availableSpace = SHRT_MAX-1-size;
			newIncrement = (short int) availableSpace * (tempIncrement / 100.0);
			newSize = size + newIncrement;

			/* If as a result of the calculations, the current size cannot be incremented, but it is still smaller than the 
			   MAXIMUM ALLOWED POSITIVE VALUE –1(aka MAX_SIZE), then the new size is set to the value of MAXIMUM ALLOWED –1 and the function proceeds */
			if (newSize >= MAX_SIZE || newIncrement == 0) {
				newSize = MAX_SIZE;
			}
			
		}

		/* Reallocating memory/space for the content character array */
		tempContent = (char*)realloc(pBuffer->content, newSize); 
		/* if reallocation unsuccessful, return NULL */
		if (tempContent == NULL) {
			return NULL;
		}
		pBuffer->content = tempContent;
		pBuffer->size = (short)newSize; /* casting newSize to short as that's what the type is in structure, this solved an error */

		/* check that the content character array memory location has changed, and reset buffer structure flags */
		if (pBuffer->content != tempContent) { 
			pBuffer->flags = bGetFlags(pBuffer) | SET_R_FLAG; 
		}
	}
	/* adding ch (character) into the content charcter array and increment the addCOffset */
	pBuffer->content[pBuffer->addCOffset] = ch; 
	pBuffer->addCOffset++;

	return pBuffer;
}

/*************************************************************************************************
* Function Name: bClean
* Purpose: To clean the buffer, re-initialize data members to default values 
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions:
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure with buffer, 0 when successful
* Algorithm: To reset buffer structures addCOffset and getCOffset to 0, and flags to default value
**************************************************************************************************/
int bClean(bPointer const pBuffer) {
	
	/* if failure with buffer, return RT_FAIL_1 */
	if (!pBuffer) {
		return RT_FAIL_1; 
	}
	
	pBuffer->addCOffset = 0;
	pBuffer->getCOffset = 0;
	pBuffer->addCOffset;
	pBuffer->flags = DEFAULT_FLAGS;

	return 0;
}

/**********************************************************************************
* Function Name: bFree
* Purpose: To free the memory/space allocated for the buffer structure
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: free()
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: 0 when successful (which is always) 
* Algorithm: To free the content character array and buffer structures memory/space
***********************************************************************************/
int bFree(bPointer const pBuffer) {
	
	/* frees the memory occupied by the character buffer and the buffer structure */
	if (pBuffer) {
		free(pBuffer->content); 
		free(pBuffer);
	}
	return 0;
}

/********************************************************************************************
* Function Name: bIsFull
* Purpose: To confirm if the character array is the buffer structure if full
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: bGetAddChOffset(), bGetSize()
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure with buffer, 1 if buffer is full, 0 if buffer has space
* Algorithm: To check if the bGetAddChOffset is the same as the bGetSize (addCOffset = size)
********************************************************************************************/
int bIsFull(bPointer const pBuffer) {
	
	/* if failure with buffer, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* check if the buffer is full or not  */
	if (bGetAddChOffset(pBuffer) == bGetSize(pBuffer)) { 
		return 1;
	}
	else {
		return 0;
	}
}

/***************************************************************************************************************
* Function Name: bGetAddChOffset
* Purpose: To get/return the buffer structures addCOffset
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure with buffer, short addCOffset - pointer to the buffer structures addCOffset
* Algorithm: n/a, simply returns addCOffset from the buffer structure
****************************************************************************************************************/
short bGetAddChOffset(bPointer const pBuffer) {
	
	//if failure, return RT_FAIL_1(-1)
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	//function returns the current addCOffSet
	return pBuffer->addCOffset;
}

/****************************************************************************************************
* Function Name: bGetSize
* Purpose: To get/return the buffer structures size
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure with buffer, short size - pointer to the buffer structures size
* Algorithm: n/a, simply returns size from buffer structure
****************************************************************************************************/
short bGetSize(bPointer const pBuffer) {
	
	/* if failure, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	
	return pBuffer->size;
}

/***************************************************************************************************
* Function Name: bGetMode
* Purpose: To get/return the buffer structures mode 
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure with buffer, char mode - pointer to the buffer structures mode
* Algorithm: n/a, simply returns mode from buffer structure
***************************************************************************************************/
int bGetMode(bPointer const pBuffer) {
	
	/* if failure with buffer, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1; 
	}

	return pBuffer->mode;
}

/***********************************************************************************************
* Function Name: bGetMarkOffset
* Purpose: To get/return the buffer structures markOffset 
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: -1 if failure with buffer, short markOffset from buffer structure if successful 
* Algorithm: n/a, simply return the markOffset from the buffer structure
***********************************************************************************************/
short bGetMarkOffset(bPointer const pBuffer) {
	
	/* if failure with buffer, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	return pBuffer->markOffset;
}

/*****************************************************************************************************************
* Function Name: bSetMarkOffset
* Purpose: To set the markOffset in the buffer structure
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure, short mark - the new mark position/offset  
* Return Value: -1 if failure with buffer, 0 if setting markOffset successsful
* Algorithm: To set the markOffset position to mark in the buffer structure
*****************************************************************************************************************/
short bSetMarkOffset(bPointer const pBuffer, short mark) {
	
	/* if failure happens, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* check that parameter mark is within the current limit of the buffer (0 to addCOffset inclusive) */
	if (mark < 0 && mark >= pBuffer->addCOffset) {
		mark = pBuffer->markOffset;
	}

	return 0;
}

/************************************************************
* Function Name: bFinish
* Purpose: To add a character to the end of the content character array
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: realloc(), sizeof(), bGetFlags()
* Parameters: bPointer const pBuffer - pointer to the buffer structure, 
              char ch - the character being added to the end of the content character array
* Return Value: NULL if failure with buffer or character array, NULL if newSize less than or equal 0, 
*               NULL if cannot reallocate memory for the new/temp content, pBuffer - the buffer if successful
* Algorithm: add one to size and ensure its positive, allocate space for the new character and confirm space created (set the r flag if needed),
             save the character into the newly created space in the content character array, save the newSize and new addCOffset to the buffer
**************************************************************/
bPointer bFinish(bPointer const pBuffer, char ch) {
	
	char* tempContent;
	short newSize;

	/* if failure with buffer or content character array, returnn NULL */
	if (!pBuffer || !pBuffer->content) {
		return NULL;
	}

	/* create the newSize */
	newSize = pBuffer->addCOffset + 1;

	/* check that newSize is still positive value */
	if (newSize <= 0) {
		return NULL;
	}

	/* reallocating memory space for the new content */
	tempContent = (char*)realloc(pBuffer->content, sizeof(char) * newSize); 
	/* ensure space is/was reserved correclty after reallocating memory */
	if (!tempContent) {
		return NULL;
	}

	/* If the size of the character buffer has changed, set the r_flag */
	if (tempContent != pBuffer->content) {
		pBuffer->flags = bGetFlags(pBuffer) | SET_R_FLAG;
		pBuffer->content = tempContent;
	}

	/* save the newSize into the size and set/put the character to the end of the buffer (->content[pBuffer->addCOffset++] = ch) */
	pBuffer->size = newSize;
	pBuffer->content[pBuffer->addCOffset++] = ch;

	return pBuffer;
}

/*****************************************************************************************************************************************
* Function Name: bDisplay
* Purpose: To loop through the entire content character array and print each character
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: bGetCh, bGetFlags
* Parameters: bPointer const pBuffer - pointer to the buffer structure, 
* Return Value: -1 if failure with buffer or character array, int characterTracker - the number of characters printed
* Algorithm: Confirm buffer and content character array exist, loop through the content character array and print/display each character, 
*            print a new line character at the end 
*****************************************************************************************************************************************/
int bDisplay(bPointer const pBuffer, char nl) {
	
	int characterTracker = 0;
	char character;

	/* if failure with buffer or character array, return RT_FAIL_1(-1) */
	if (!pBuffer || !pBuffer->content) {   
		return RT_FAIL_1;
	}
	
	/* Loop to print the content of the buffer calling bGetCh() and
	   checking the flags in order to detect the end of the buffer content */
	do {
		character = bGetCh(pBuffer);
		if ((int)(bGetFlags(pBuffer) & CHECK_EOB)) {
			break;
		}
		/*  prints character by character the contents of the character buffer */
		printf("%c", character);
		characterTracker++;
	} while (!((bGetFlags(pBuffer) & CHECK_EOB)));

	/* checks nl and if it is not 0, print a new line character */
	if (nl != 0) {
		printf("\n");
	}
 
	return characterTracker;
}

/********************************************************************************************************
* Function Name: bLoad
* Purpose: To Load the contents of a file into the buffer structure
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions:
* Parameters: bPointer - pointer to the buffer structure, FILE* const fi - the file being read in
* Return Value: -1 if failure with buffer or file, -2 if cannot load a character from file into buffer, 
*               char characterTracker - the number of characters added into the content character array
* Algorithm: to continue reading from the file until the end of file symbol is detected
********************************************************************************************************/
int bLoad(bPointer const pBuffer, FILE* const fi) {
	
	char tempCharacter;
	int characterTracker = 0;

	/* if failure with buffer structure or file, return RT_FAIL_1(-1) */
	if (!pBuffer || !fi) {
		return RT_FAIL_1; 
	}

	/* continue until the EOF is detected */
	while (!feof(fi)) {
		/* get each of the characters */ 
		tempCharacter = (char)fgetc(fi);
		/* if the character is the EOF, break out of loop */
		if (feof(fi)) {
			break;
		}

		/* try to add the read-in character to the buffer (-2 if cannot,  increment the characterTracker if can) */
		if (!bAddCh(pBuffer, tempCharacter)) {
			ungetc(tempCharacter, fi);
			return LOAD_FAIL;
		}
		else {
			characterTracker++; 
		}
	}

	return characterTracker; 
}

/***************************************************************************************************************************************************
* Function Name: bIsEmpty
* Purpose: To confirm/check if the content character array in the buffer structure is empty
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: -1 if failure happens, 1 if the content character array is empty, 0 if the conent character array is not empty
* Algorithm: checks if the addCOffset is 0 (confirms nothing added, so content character array is empty)
****************************************************************************************************************************************************/
int bIsEmpty(bPointer const pBuffer) {
	
	/* if failure happens, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* check if the addCOffset is 0, return 1. otherwise return 0 */
	if (pBuffer->addCOffset == 0) {   
		return 1;
	}
	else {
		return 0;
	}
}


/*********************************************************************************************************************************************
* Function Name: bGetCh
* Purpose: To get/return a character from the content character array depending/based on the buffer structures getCOffsest
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: bGetAddChOffset, bGetFlags
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: -1 if failure happens, 0 if the getCOffset and addCOffset are equal, char tempCharacter locaed at getCOffset
* Algorithm: To confirm if at the end of the character array, to return the character index of the getCOffset in the content character array, 
*            and to increment the gettCOffset by 1
*********************************************************************************************************************************************/
char bGetCh(bPointer const pBuffer) {
	
	char tempCharacter;

	/* if failure happens, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1; 
	}

	/* check to see if getCOffset and addCOffset are equal, if they are set the flags field eob_flag bit to 1 */
	if (pBuffer->getCOffset == bGetAddChOffset(pBuffer)) {  
		pBuffer->flags = bGetFlags(pBuffer) | SET_EOB;  
		return 0;
	}
	/* otherwise, it sets eob_flag to 0 */
	else {
		pBuffer->flags = bGetFlags(pBuffer) & RESET_EOB;  
	}

	/* increment getCOffset by 1 and return the character located at getCOffset */
	tempCharacter = pBuffer->content[pBuffer->getCOffset]; 
	pBuffer->getCOffset++;

	return tempCharacter;
}

/************************************************************
* Function Name: bRewind
* Purpose: To re-set the buffer structures getCOffset and markOffset to 0, so buffer can be re-read again
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: -1 if failure happens, 0 when successful
* Algorithm: set getCOffset and markOffset to 0
**************************************************************/
int bRewind(bPointer const pBuffer) {
	
	/* if failure happens, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;

	return 0;
}

/**************************************************************************************************************
* Function Name: bRetract
* Purpose: To decrease the buffer structures getCOffset by one
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: bGetAddChOffset
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: NULL if failure happens, the actual buffer structure if successful
* Algorithm: decrease the getCOffset by 1 if the addCOffset is greater than 0
***************************************************************************************************************/
bPointer bRetract(bPointer const pBuffer) {
	
	/* if failure happens, return NULL */
	if (!pBuffer) {
		return NULL;
	}
	/* decrease the getCOffset by 1 if the addCOffset is greater than 0 */
	if (bGetAddChOffset(pBuffer) > 0) {
	pBuffer->getCOffset--;
	}

	return pBuffer;
}

/***************************************************************************************************************
* Function Name: bRestore
* Purpose: To set/restore the buffer structures getCOffset
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: -1 if failure happens or no space in buffer, short getCOffset from buffer if successful
* Algorithm: Confirms if there is space, then sets the getCOffset to the markOffset and returns the getCOffset
***************************************************************************************************************/
short bRestore(bPointer const pBuffer) {
	
	/* if failure happens, return -1 */
	if (!pBuffer) {
		return -1;
	}
	
	/* need to confirm if there is space */
	if (pBuffer->getCOffset > pBuffer->addCOffset || pBuffer->getCOffset > pBuffer->size) {
		return -1;
	}

	pBuffer->getCOffset = pBuffer->markOffset;

	return pBuffer->getCOffset;
}

/*******************************************************************************************************
* Function Name: bGetChOffset
* Purpose: To get/return the character offset to the get-character location within the content characer array
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure happens, short getCOffset from buffer if successful
* Algorithm: n/a, simply to return the buffer structures getCOffset
*******************************************************************************************************/
short bGetChOffset(bPointer const pBuffer) {
	
	/* if failure happens, return RT_FAIL_1(-1) */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	return pBuffer->getCOffset;
}

/************************************************************************************************************************************
* Function Name: bGetIncrement
* Purpose: To get/return the increment value from the buffer structure 
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_INC_FAIL if failure happens, size_t increment - the increment value in the buffer structure (cast to unsigned char 
*               according to data type if successful)
* Algorithm: to return the the buffer structures increment value
************************************************************************************************************************************/
size_t bGetIncrement(bPointer const pBuffer) {
	
	/* if failure happens, return RT_INC_FAIL(0x100) */
	if (!pBuffer) {
		return RT_INC_FAIL;
	}

	/* returns the non-negative value of increment to the calling function. The return datatype(size_t) 
	   must be appropriately checked according to datatype you are getting */ 
	return (size_t)(unsigned char)pBuffer->increment;
}

/*************************************************************************************************************************
* Function Name: bGetContent
* Purpose: To get/return the buffers content character array with the character positions offset 
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure, 
*             short chPosition - the character number offset from the start of the content(character array)
* Return Value: NULL if failure happens or if character position is negative or larger than the addCOffset, 
*               char* pointer to the buffer structure content character array with the character offset added if successful
* Algorithm: the sum of the buffers character array and character position offset
***************************************************************************************************************************/
char* bGetContent(bPointer const pBuffer, short chPosition) {
	
	/* if failure happens, return NULL */
	if (!pBuffer) {
		return NULL;
	}

	/* check to see if the character position is negative or larger than the addCOffset */ 
	if (chPosition < 0 || chPosition >= pBuffer->addCOffset) {
		return NULL;
	}

	return pBuffer->content + chPosition;
}

/****************************************************************************************************************
* Function Name: bufferAddCPosition
* Purpose: Return a pointer to the buffers character off set to the calling function
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: RT_FAIL_1 if failure happens, short pointer to buffer structure getCOffset - character offset 
*               to the add-character location if successful
* Algorithm: to return the buffers character offset 
****************************************************************************************************************/
short bufferAddCPosition(bPointer const pBuffer) { 
	
	/* if failure happens, return RT_FAIL_1(-1) */ 
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* returns getCOffset to the calling function */ 
	return pBuffer->getCOffset;
}

/**************************************************************************************************************************************
* Function Name: bGetFlags
* Purpose: To get/return the buffer structures flags value to the calling function
* Author: Patrick Czermak
* History/Versions: 1
* Called Functions: n/a
* Parameters: bPointer const pBuffer - pointer to the buffer structure
* Return Value: unsigned short RT_FAIL_1 if failure happens, unsigned short pointer to buffer structure flags field - (character array 
*               reallocation and end-of-buffer flag)
* Algorithm: to return the buffers current flags value
***************************************************************************************************************************************/
unsigned short bGetFlags(bPointer const pBuffer) {
	
	/* if failure happens, return RT_FAIL_1(-1) */ 
	if (!pBuffer) {
		return (unsigned short)RT_FAIL_1; /* cast to unsigned short to fix errors */
	}

	/* returns the flag field from buffer structure */ 
	return pBuffer->flags;
}

/* END OF CLASS FILE */