/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2010  R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not,  a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <stdlib.h> /* for exit */
#include <string.h>
#include <stdio.h>

extern char *getRHOME(int); /* in ../rhome.c */

static void Usage (char *RCMD, char *arch)
{
    fprintf(stderr, "%s %s %s", "Usage:", RCMD, "[command args]\n\n");
    fprintf(stderr, "%s%s%s",
	    "where 'command args' can be\n\n",
	    "  --arch n   for n=i386, x64, 32 or 64\n",
	    "  any other arguments listed by ");
    fprintf(stderr, "%s --arch %s --help\n", RCMD, arch);
}

#define CMD_LEN 10000
#define MXA 20
int main (int argc, char **argv)
{
    int cmdarg = 1;
    char arch[MXA] = R_ARCH, cmd[CMD_LEN], *p;

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
	Usage("Rscript", arch);
	exit(0);
    }
    
    if (argc > 1 && strcmp(argv[1], "--arch") == 0) {
	cmdarg = 3;
	if(argc < 3) {
	    Usage("Rscript", arch);
	    exit(0);
	}
	strncpy(arch, argv[2], MXA); arch[MXA-1] = '\0';
	if(strcmp(arch, "32") == 0) strcpy(arch, "i386");
	if(strcmp(arch, "64") == 0) strcpy(arch, "x64");
	if(strcmp(arch, "i386") && strcmp(arch, "x64")) {
	    fprintf(stderr, "valid values for --arch are i386, x64, 32, 64\n");
	    exit(1);
	}
    } else if ((p = getenv("R_ARCH")))
	strncpy(arch, p+1, 10); /* skip leading slash */
    

    snprintf(cmd, CMD_LEN, "%s\\bin\\%s\\Rscript.exe", getRHOME(2), arch);

    for(int i = cmdarg; i < argc; i++) {
	strcat(cmd, " ");
	if(strchr(argv[i], ' ')) {
	    strcat(cmd, "\"");
	    /* We should really escape " here, I believe */
	    strcat(cmd, argv[i]);
	    strcat(cmd, "\"");
	} else strcat(cmd, argv[i]);
    }
    
    exit(system(cmd));
 }
