#define NULL 0
typedef unsigned size_t;

int main(int argc, char **argv);
int Tprintf(const char * fmt, ...);


#if (__WATCOMC__)
//#pragma data_seg ( _DATA_startup, TDATA )
//#pragma code_seg ( _TEXT_startup, TCODE )
#endif


/*
__I4M
__U4D
_cstart_
intdos
*/

int _argc = 0;
extern char * _heap;
extern char * _environ;

void *malloc(size_t size) 
{
	char *p = _heap;
	_heap += size;
	return p;
}


/* returns nonzero if c is a whitespace character - 
   space, horizontal tab, line feed, carriage return, form feed, or vertical tab 
 */
int isspace(int c)
{
	return (c==' ') || (c=='\t') || (c=='\n') || (c=='\r') || (c=='\f') || (c=='\v');
}

/* returns nonzero if c is a control character, c */
int iscntrl(int c)
{
	return (c <= 0x1f) || (c==0x7f);
}

#define isargdelim(c) (isspace(c) || iscntrl(c) || (c==',') || (c==';') || (c=='='))
extern char sstack;

/* returns DOS exit code if doesn't stay resident */
int _cstart(void) 
{
	char *cmdline = &sstack;  /* 0x81 is offset of command line in _psp, copied to start of stack */
	char *argv[9];
	char *p;
	int i;
	void writechar(char c);
	writechar('*');
	for (p = cmdline, i = 1, argv[0]=cmdline; *p && (*p!='\r'); p++)
	{
		register char c = *p;
		if (isargdelim(c))
		{
			/* terminate argv[i] */
			*p = '\0';  
			/* skip whitespace and other characters to advance to start of next argument (or reach end) */
			for(p++; *p && (*p!='\r') && isargdelim(*p); p++) {}
			if (*p && (*p!='\r')) 
			{
				/* get start of next argument */
				argv[i] = p;
				i++;
			}
		}
	}
	*p = '\0';
	_argc = i;
	
	Tprintf("argc = %i\n", _argc);
	Tprintf("cmdling=[%s]\n", cmdline);
	for (i = 0; i < _argc; i++)
	{
		Tprintf("argv[%i]=[%s]\n", i, argv[i]);
	}
	
	return main(_argc, argv);
}


int strlen(char *s) 
{
  int i = 0;

  while (*s++)
    i++;

  return i;
}

void * memcpy(void * src, void * dst, size_t len) 
{
  int i;
  register char *s, *d;
  for (i = 0, s=(char *)src, d=(char *)dst; i < len; i++, s++, d++)
  {
    *d = *s;
  }
  
  return dst;
}

char * strcpy(char * src, char * dst) 
{
  for ( ; *src; src++, dst++)
  {
    *dst = *src;
  }
  *dst = *src;

  return dst;
}

char * strcat(char * src, char * dst) 
{
  strcpy(src, dst+strlen(dst));
  return dst;
}

/* returns pointer to first match to c or NULL if not in str */
char * strchr(const char *str, int c) {
	const char *p;
	for (p = str; *p; p++)
		if (*p == (char)c) return (char *)p;
	return NULL;
}

/* sets memory to val */
void * memset(void *src, int val, size_t size) {
	char *p;
	size_t i;
	for (p = src, i=0; i < size; i++)
		*p = (char)val;
	return src;
}



// used by kitten

char *getenv(char *s) {return NULL;}

//int open(const char *filename, int mode, ... );
int read(int fileno, int count, int block_size) { return 0; }
int write() {return 0;}
