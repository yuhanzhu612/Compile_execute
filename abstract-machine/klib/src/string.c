#include <klib.h>
#include <klib-macros.h>
#include <stdint.h>

#if !defined(__ISA_NATIVE__) || defined(__NATIVE_USE_KLIB__)

size_t strlen(const char *s) {
  panic("Not implemented");
}

char *strcpy(char *dst, const char *src) {
 // panic("Not implemented");
    assert(NULL != dst);
    assert(NULL != src);
    if (NULL ==dst || NULL == src)
         return NULL;
    char* ret = dst;
    while((*dst++ = *src++) != '\0') ;
    return ret;
}

char *strncpy(char *dst, const char *src, size_t n) {
  panic("Not implemented");
}

char *strcat(char *dst, const char *src) {
 // panic("Not implemented");
char* address=dst;
assert( (dst!=NULL)&&(src!=NULL) );
while(*dst)
{
dst++;
}

while((*dst++ = *src++) != '\0')
{
NULL;
}
return address;
}

int strcmp(const char *s1, const char *s2) {
//  panic("Not implemented");
  int j;
  int i;
    for (i = 0;((*(s1 + i) != '\0') && (*(s2 + i) != '\0')); i++)
    {
        j= *(s1 + i) - *(s2 + i);
        if (j > 0)
        {
            return 1;
        }
        else if (j==0)
        {
            return 0;
        }
        else
        {
            return -1;
        }
    }
    return 0; 
}

int strncmp(const char *s1, const char *s2, size_t n) {
  panic("Not implemented");
}

void *memset(void *s, int c, size_t n) {
//  panic("Not implemented");
 	 char *p = (char *)s;
    while (n--) {
        *p++ = c;
    }   
    return s;
}

void *memmove(void *dst, const void *src, size_t n) {
  panic("Not implemented");
}

void *memcpy(void *out, const void *in, size_t n) {
//  panic("Not implemented");
  if (NULL == out || NULL == in || n < 0)
		return NULL;
	char *tempDest = (char *)out;
	char *tempSrc = (char *)in;
 
	while (n-- > 0)
		*tempDest++ = *tempSrc++;
	return out;	
}

int memcmp(const void *s1, const void *s2, size_t n) {
//  panic("Not implemented");
  assert((NULL != s1) && (NULL != s2));
	
	const char *pstr1 = (const char*)s1;
	const char *pstr2 = (const char*)s2;
	
	while (n--) {
	    if (*pstr1 && *pstr2 && (*pstr1 == *pstr2)) {
		continue;
	    } else {
		break;
	    }
	}
	return (*pstr1 - *pstr2);
}

#endif
