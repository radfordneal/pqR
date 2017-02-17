/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Facility for maintaining sets of objects - test program

   Copyright (c) 2016, 2017 Radford M. Neal.

   The SGGC library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <stdlib.h>
#include <stdio.h>
#include "set-app.h"

#define N_SET 3
struct set set[N_SET];

int main (void)
{ 
  FILE *f;
  int i, j, x, o, r;
  char s[1000];
  char c; 

  f = fopen ("script","r");
  if (f == NULL) 
  { fprintf(stderr,"No script file\n");
    exit(1);
  }

  for (j = 0; j<N_SEG; j++)
  { set_segment_init (&segment[j]);
  }
  for (i = 0; i<N_SET; i++) 
  { set_init (&set[i], i<SET_CHAINS ? i : SET_CHAINS-1);
  }

  /* Loop to do commands from script.  Commands are:

        c  set  index  offset        set_contains
        a  set  index  offset        set_add
        r  set  index  offset        set_remove
        b  set  index  offset        set_assign_segment_bits (to 7)
        n  set  index  offset        set_move_next (to other of set 1/2)
        m  set                       set_move_first (to other of set 1/2)
        A  set  index  offset        set_add_segment (from chain 0)
        R  set  index  offset        set_remove_segment (from chain 0)
  */

  for (;;)
  { 
    /* Read a command. */

    printf("> ");
    c = ' ';
    i = x = o = -1;
    r = fscanf(f," %c %d %d %d",&c,&i,&x,&o);
    if (r == -1)
    { printf("\n");
      return 0;
    }

    s[0] = 0;
    fscanf(f,"%[^\n]",s);

    if (c == 'm')
    { if (r != 2)
      { printf("Wrong number of arguments\n");
      }
      printf("%c %d   %s\n",c,i,s);
    }
    else
    { if (r != 4)
      { printf("Wrong number of arguments\n");
      }
      printf("%c %d %d %d  %s\n",c,i,x,o,s);
    }

    if (i < 0 || i >= N_SET) 
    { printf("Invalid set\n");
      continue;
    }

    if (c != 'm')
    { if (x < 0 || x >= N_SEG) 
      { printf("Invalid segment\n");
        continue;
      }
      if (o < 0 || o >= 1<<SET_OFFSET_BITS) 
      { printf("Invalid offset\n");
        continue;
      }
    }

    /* Do the command. */

    switch (c) 
    { case 'c': 
      { printf("result: %d\n", set_contains (&set[i], SET_VAL(x,o)));
        break;
      }
      case 'a':
      { printf("result: %d\n", set_add (&set[i], SET_VAL(x,o)));
        break;
      }
      case 'r':
      { printf("result: %d\n", set_remove (&set[i], SET_VAL(x,o)));
        break;
      }
      case 'b':
      { set_assign_segment_bits (&set[i], SET_VAL(x,o), 7);
        break;
      }
      case 'n':
      { set_move_next (&set[i], SET_VAL(x,o), &set [i==1 ? 2 : 1]);
        break;
      }
      case 'm':
      { set_move_first (&set[i], &set [i==1 ? 2 : 1]);
        break;
      }
      case 'A':
      { set_add_segment (&set[i], SET_VAL(x,o), 0);
        break;
      }
      case 'R':
      { set_remove_segment (&set[i], SET_VAL(x,o), 0);
        break;
      }
      
      default: 
      { printf("Unknown operation");
        break;
      }
    }

    /* Show the contents of all the sets. */

    for (i = 0; i<N_SET; i++)
    { set_value_t v;
      printf ("Set %d (chain %d), %d elements:",
              i, set_chain(&set[i]), set_n_elements(&set[i]));
      v = set_first (&set[i], 0);
      if (v == SET_NO_VALUE)
      { printf(" empty\n");
        continue;
      }
      printf (" %016llx :", (long long) set_first_bits (&set[i]));
      while (v != SET_NO_VALUE)
      { printf(" %d.%d",SET_VAL_INDEX(v),SET_VAL_OFFSET(v));
        v = set_next (&set[i], v, 0);
      }
      printf("\n");
    }
  }
}
