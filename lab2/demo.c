/* The DEMO parser */

#include "demo.h"

char *CodeBase = "R02.02";

const char *const type_name[] =
{
"UNKNOWN", "INT", "CHAR"
};

static char *InputFile;

static void help_message( char *p )
{
    fprintf(stdout, "Syntax: %s [-d] [filename]\n", p);
    fprintf(stdout, "\t-d increments debugging flag.\n");
    fprintf(stdout, "\tif 'filename' is omitted %s reads from 'stdin'.\n\n", p);
    exit(-1);
}

static void truncate_filename( char *filename )
{
    char *p;
    int count;

    p = filename;
    count = 0;

    while (*p != '\0') {
        if (*p == '.')
            count++;
        p++;
    }
    if (count > 0) {
        while (p != filename) {
            if (*p == '.') {
                *p = '\0';
                p--;
                break;
            }
            p--;
        }
    }
    if (Debug > 1)
        fprintf(LOG, "Input file basename is '%s'.\n", filename);
}

static void Initialize( char *bn )
{
    char fn[128];

    fprintf(stdout, "COMP 506 DEMO Compiler\n");

    /* for the moment, make stdout unbuffered */
    Setup(stdout, "stdout", 1);
    fprintf(LOG, "\n");
}

char *TypeNames[] = {"unknown", "int", "char", "boolean", "procedure"};

int main( int argc, char **argv )
{
    int count, file;
    char *p, *filename;
    FILE *infile;

    LOG = fopen("./LogFile", "w");
    SetupAndMark(LOG, "LogFile", 0);
    fprintf(LOG, "File disposition:\n");

    file = 0;
    Debug = 0;
    syntax_error = 0;
    Verbose = 0;
    TrackValues = 0;
    TrackRegisters = 0;
    TrackBuffers = 0;
    LastPreassignedRegister = 0;

    count = 1;

    while (count < argc) {
        if (*argv[count] == '-') {
            p = argv[count];
            p++;
            switch (*p) {
                case 'd':
                    Debug++;
                    break;
                default:
                    fprintf(stderr, "Command-line flag '%c' not recognized.\n", *p);
                    help_message(argv[0]);
                    break;
            }
        } else { /* a filename */
            if (file > 0)
                help_message(argv[0]);

            file = 1;
            filename = ssave(argv[count]);
            fprintf(LOG, "\topening file '%s'\n", filename);
            infile = freopen(filename, "r", stdin);
            if (infile == NULL) {
                fprintf(LOG, "\tFile open of '%s' failed. Reading stdin instead.\n",
                        filename);
                fprintf(stderr, "File open of '%s' failed. Reading stdin instead.\n",
                        filename);
                file = 0;
            } else {
                InputFile = ssave(filename);
                truncate_filename(filename);
                stdin = infile;
            }
        }
        count++;
    }

    if (file == 0) {
        filename = ssave("stdin");
        InputFile = filename;
        fprintf(LOG, "\tReading from stdin.\n");
    }

    Initialize(filename);

    yyparse();

    if (syntax_error == 0) {
        fprintf(stderr, "Parser succeeds.\n");
    } else { /* first, complain */
        fprintf(stderr, "\nParser fails with %d error messages.\nExecution halts.\n",
                syntax_error);
        fprintf(LOG, "\nParser fails with %d error messages.\nExecution halts.\n",
                syntax_error);

        die(); /* then, quit */
    }
}

void die()
{
    /* delete the CODE and SLFILE files */
    fprintf(LOG, "Due to errors, deleting files %s and %s\n",
            code_file_name, sl_file_name);
    exit(-1);
}

char *ssave( char *s )
{
    char *ns = (char *) malloc(strlen(s) + 1);
    (void) strcpy(ns, s);
    return s;
}

void sfree( char *s )
{
    free(s);
}

struct NODE *make_leaf_node( int node_type, char *var_name, int var_value)
{
    struct NODE *n = (struct NODE *) malloc(sizeof(struct NODE));

    n->node_type = node_type;

    n->var_name = var_name;
    n->var_val = var_value;
    if(node_type == TYPE_INT || node_type == TYPE_CHAR){
        n->var_type = node_type;
    }else{
        if (getsym(var_name) != NULL){
           n->var_type = getsym(var_name)->type;
        }
        else{
           yyerror("error:Undeclared variable\n ");
           die();
        }
    }

    n->child_num = 0;
    return n;
}

struct NODE *make_op_node( int node_type, struct NODE *children[], int child_num)
{
    // Do type checking in "compile time"
    if( NODE_PLUS < node_type && node_type < NODE_DIVIDE){
        if (children[0]->var_type != children[0]->var_type) {
            char str[80];
            sprintf(str, "Not possible opeartion %d, between types %s and %s \n", node_type, type_name[children[0]->var_type],
                    type_name[children[1]->var_type]);
            yyerror(str);
        }
    }

    struct NODE *n = (struct NODE *) malloc(sizeof(struct NODE));

    n->node_type = node_type;

    n->var_name = "";

    n->var_type = children[0]->var_type; //take var_type from first child - useful for binary op, not for others

    n->child_num = child_num;
    int i;
    for (i = 0; i < child_num; ++i) {
        n->children[i] = children[i];
    }

    return n;
}

void add_sybling_node( struct NODE *root, struct NODE *sybling){
    if(root->child_num + 1 > CHILD_NUM){
        char str[80];
        sprintf(str, "Number of statements larger than CHILD_NUM = %d\n",CHILD_NUM);
        yyerror(str);
        return;
    }
    root->children[root->child_num++] = sybling;

}

struct NODE *copy_node( struct NODE *orig )
{
    if (orig == NODENULL) { return NODENULL; }
    struct NODE *copy = (struct NODE *) malloc(sizeof(struct NODE));
    copy->node_type = orig->node_type;
    copy->var_type = orig->var_type;
    copy->var_val = orig->var_val;
    int i;
    for ( i = 0; i < CHILD_NUM; ++i) {
        copy->children[i] = orig->children[i];
    }
    return copy;
}

struct symrec *putsym( char const *name, int const type, int val)
{
    struct symrec *res = (struct symrec *) malloc(sizeof(struct symrec));
    res->name = strdup(name);
    res->type = type;
    res->val = val;
    res->next = sym_table;
    sym_table = res;
    return res;

}

struct symrec *getsym( char const *name )
{
    struct symrec *p;
    if (sym_table == NULL) { return NULL; }
    for (p = sym_table; p; p = p->next) {
        // printf ("in table: %s, value to find: %s\n", p->var_name, var_name);
        if (strcmp(p->name, name) == 0) {
            return p;
        }
    }
    return NULL;
}

void update_st(char *name, int type, int value){
    // TODO:check assignment type: char <- int NO, int <- char OK if char =[0-9]

    struct symrec *current = getsym(name);

    if (!current){
        yyerror("error: Undeclared variable/n");
        die();
    }else if(current->type == TYPE_CHAR && type == TYPE_INT){
        yyerror("error:Invalid transfer, can't transfer from INT to CHAR\n");
        die();
    }
    else if(current->type == TYPE_INT && type == TYPE_CHAR){
        //printf("test2%d",value);
        if (value>=48 && value<=57){
        current->val = value - 48;
        }
        else{
        yyerror("error:Invalid transfer, can't assign from CHAR to INT except from char=[0-9]\n");
        die();
        }
    }
    else{
        current->val = value;
    }

    return;
}

int transfer(char* string){
    int res;
    //printf("a%s",string);
    if (string[1]>='0' && string[1]<='9'){
        //printf("b%c",string[1]);    
        //printf("c%d",atoi(string));
        res = (int)string[1];
        //printf("test1%d\n",res);
    }
    else if(string[1] == '\\'){
        switch(string[2]){
            case 'a': 
                 res = 300;
                 break;
            case 'b':
                 res = 301;
                 break;
            case 'n':
                 res = 302;
                 break;
            case 'r':
                 res = 303;
                 break;
            case 't':
                 res = 304;
                 break;
            case 'v':
                 res = 305;
                 break;
            case '0':
                 res = 306;
                 break;
            default:
                 yyerror("error:Invalid CHAR\n");
                 die();
                 break;
        }
       }
       else{
        res = (int)string[1];
        //printf("f%d",res);
        }
     //printf("test2%d \n",res);
     return res;
    
}
void walk( struct NODE *root )
{
    int i;
    if (!root){
        return;
    }
    switch (root->node_type) {
        case NODE_STATEMENTS:

            for ( i = 0; i < root->child_num ; ++i) {
                walk(root->children[i]);
            }
            return;
        case NODE_NAME:
            if(getsym(root->var_name)!=NULL){
                root->var_val = getsym(root->var_name)->val;
                }
            else{
                yyerror("error:Undeclared variable\n");
                die();
            }
            return;
        case NODE_PLUS:
            walk(root->children[0]);
            walk(root->children[1]);
            if (root->children[0]->var_type == TYPE_INT && root->children[1]->var_type == TYPE_INT){
                if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
                root->var_val = root->children[0]->var_val + root->children[1]->var_val;
            }
            else{
                yyerror("error:Invaild PLUS operation, inconsistent types, can't PLUS with CHAR\n"); 
                die();
            }
            return;
        case NODE_MINUS:
            walk(root->children[0]);
            walk(root->children[1]);
            if (root->children[0]->var_type == TYPE_INT && root->children[1]->var_type == TYPE_INT){
                if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
                root->var_val = root->children[0]->var_val - root->children[1]->var_val;
                }
            else{
                yyerror("error:Invaild MINUS operation, inconsistent types, can't MINUS with CHAR\n"); 
                die();
            }
            return;
        case NODE_TIMES:
            walk(root->children[0]);
            walk(root->children[1]);
            if (root->children[0]->var_type == TYPE_INT && root->children[1]->var_type == TYPE_INT){
                if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
                root->var_val = root->children[0]->var_val * root->children[1]->var_val;
                }
            else{
                yyerror("error:Invaild Multiply operation, inconsistent types, can't MULTIPLY with CHAR\n"); 
                die();
            }
            return;
        case NODE_DIVIDE:
            walk(root->children[0]);
            walk(root->children[1]);
            if (root->children[0]->var_type == TYPE_INT && root->children[1]->var_type == TYPE_INT){
                if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
                if (root->children[1]->var_val == 0){
                   yyerror("error:Devided by zero\n");
                   die();
                }
                else{
                root->var_val = root->children[0]->var_val / root->children[1]->var_val;
                } 
            }
            else{
                yyerror("error:Invaild DIVIDE operation, inconsistent types, can't DIVIDE with CHAR\n"); 
                die();
            }
            return;
        case NODE_ASSIGN:
            walk(root->children[1]);
            if(root->children[1]->var_val== INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            update_st(root->children[0]->var_name, \
                        root->children[1]->var_type,root->children[1]->var_val);
            return;
        case NODE_WRITE:
            walk(root->children[0]);
            if(root->children[0]->var_val== INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if (root->children[0]->var_type == TYPE_CHAR){
                switch(root->children[0]->var_val){
                   //abnrtv0
                   case 300:
                       printf("Write: \'\\a\'\n");
                       break;
                   case 301:
                       printf("Write: \'\\b\'\n");
                       break;  
                   case 302:
                       printf("Write: \'\\n\'\n");   
                       break;
                   case 303:
                       printf("Write: \'\\r\'\n");   
                       break;
                   case 304:
                       printf("Write: \'\\t\'\n");
                       break;
                   case 305:
                       printf("Write: \'\\v\'\n");
                       break;
                   case 306:
                       printf("Write: \'\\0\'\n");  
                       break;
                   default:
                       printf("Write: \'%c\'\n", root->children[0]->var_val);
                       break;
                   }
                }
                else if(root->children[0]->var_type == TYPE_INT){
                       printf("Write: %d\n", root->children[0]->var_val);
                   }
                
            return;
        case NODE_WHILE:
            walk(root->children[0]);
            if(root->children[0]->var_val== INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }            
            while (root->children[0]->var_val){
                walk(root->children[1]);
                walk(root->children[0]);
            }
            return;
        case NODE_BOOL:
            walk(root->children[0]);
            if(root->children[0]->var_val== INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            root->var_val = root->children[0]->var_val;
            return;
        case NODE_LT:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val < root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_LE:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val <= root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_EQ:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val == root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_NE:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val != root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_GE:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val >= root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_GT:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid comparison between two different types\n");
                        die();
            }
            if(root->children[0]->var_val > root->children[1]->var_val){
                 root->var_val = 1;
            }
            else{
                 root->var_val = 0;
            }
            return;
        case NODE_AND:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid operation between two different types\n");
                        die();
            }
            if(root->children[0]->var_val & root->children[1]->var_val){
                  root->var_val = 1;
            }else{
                  root->var_val = 0;
            }
            return;
        case NODE_OR:
            walk(root->children[0]);
            walk(root->children[1]);
            if(root->children[0]->var_val== INT_MAX || root->children[1]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_type != root->children[1]->var_type){
                        yyerror("error: Invalid operation between two different types\n");
                        die();
            }
            if(root->children[0]->var_val | root->children[1]->var_val){
                root->var_val = 1;
            }else{
                root->var_val = 0;
            }
            return;
        case NODE_NOTBOOL:
            walk(root->children[0]);
            if(root->children[0]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }         
            if(root->children[0]->var_val){
                root->var_val = 0;
            }else{
                root->var_val = 1;
            }
            return;
        case NODE_IF:
            walk(root->children[0]);
            if(root->children[0]->var_val == INT_MAX){
                        yyerror("error: Unassigned variable\n");
                        die();
                }
            if(root->children[0]->var_val){
               walk(root->children[1]);
            }else{
               walk(root->children[2]);
            }
            return;
    }
    return;
}


void printNode( struct NODE *n )
{

    if (n == NODENULL) {
        printf("No Node \n");
        return;
    }
    printf("Node: \n");
    printf("node_type = %d\n", n->node_type);
    printf("var_val = %d\n", n->var_val);
    printf("var_type = %d\n", n->var_type);
    printf("Children : %u\n");
    int i;
    for ( i = 0; i < CHILD_NUM; ++i) {
        printf("%d child = %u\n\n", n->children[i]);
    }
}

void printRec( struct symrec *r )
{
    printf("Record: \n");
    printf("var_name = %s\n", r->name);
    printf("node_type = %d\n", r->type);
    printf("var_value = %s\n", r->val);
    printf("next = %u\n", r->next);


}

void freeAST(struct NODE *root){
    int i;
    if(root != NULL){
      for ( i = 0; i < root->child_num; ++i) {
        freeAST(root->children[i]);
      }
      free(root);
    }

}
void freeST(struct symrec *rec){
    if (rec != NULL){
        freeST(rec->next);
        free(rec->name);
        free(rec);
    }
}
