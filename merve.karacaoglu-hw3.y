%{
#ifdef YYDEBUG
  yydebug = 1;
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int line_num;

int line_no = 0;

int result;

int yylex();
void yyerror(const char *s) {}

typedef struct Identifier {
  char *name;
  int lineno;
  char *type;
  int used;
  int assigned;
  int value;
  struct Identifier *next;
} Identifier;

typedef struct ErrorNode {
    int lineno;               
    int appearance_order;   
    char *message;        
    struct ErrorNode *next;
} ErrorNode;

ErrorNode *error_list = NULL;
int current_error_order = 0; 


Identifier *identifier_table = NULL;
char *current_declaration_type = NULL;

typedef struct Evaluation{
  char * circuit_name;
  char * output_name;
  int output_result;
  struct Evaluation * next;
} Evaluation;

Evaluation * evaluation_head = NULL;

Identifier *find_identifier(const char *name);
void declare_identifier(const char *name, char *type, int line);
void unused_identifiers();
void identifier_type(const char *type);
void reset_assignments();
void print_identifier_table();
void add_error(int lineno, const char *message);
void print_errors();
void free_errors();
Identifier* reverse_list(Identifier *head);
void print_evaluations(const char * cir_name);
%}

%union {
  char *str;
  int line;
  int val;
}

%type <str> identifierList
%type <val> expression 
%token <val> tTRUE tFALSE
%token <str>  tOR tAND tXOR tNOT tLPR tRPR
%token <line> tINPUT tOUTPUT tNODE tEVALUATE tASSIGNMENT tCOMMA
%token <str> tIDENTIFIER
%left tOR tXOR
%left tAND
%precedence tNOT

%start program

%%

program : lcd
;

lcd : 
    | declarations_block circuitDesign_block evaluations_block
;


declarations_block : declaration 
             | declaration declarations_block
;

declaration : input 
            | output 
            | node
;

input : tINPUT { 
    current_declaration_type = "input"; 
    line_no = $1;
} identifierList;

node : tNODE { 
    current_declaration_type = "node"; 
    line_no = $1;
} identifierList;

output : tOUTPUT { 
    current_declaration_type = "output"; 
    line_no = $1;
} identifierList;



identifierList : tIDENTIFIER {
    Identifier *existing = find_identifier($1);
    if (existing) {
      if(strcmp(existing -> type, "input") == 0 || strcmp(existing -> type, "output") == 0){
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already declared as an %s.", line_no, $1, existing->type);
        add_error(line_num, error_message);
      }
      else{
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already declared as a %s.", line_no, $1, existing->type);
        add_error(line_num, error_message);
      }
  } 
  else {
    declare_identifier($1, current_declaration_type, line_no);
  }
}
| tIDENTIFIER tCOMMA identifierList {
    Identifier *existing = find_identifier($1);
    if (existing) {
      if(strcmp(existing -> type, "input") == 0 || strcmp(existing -> type, "output") == 0){
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already declared as an %s.", line_no, $1, existing->type);
        add_error(line_num, error_message);
      }
      else{
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already declared as a %s.", line_no, $1, existing->type);
        add_error(line_num, error_message);
      }
  } 
  else {
    declare_identifier($1, current_declaration_type, line_no);
  }
}
;


//circuit design part

circuitDesign_block : assignment 
| assignment circuitDesign_block ;



assignment : tIDENTIFIER tASSIGNMENT  {
    Identifier *ident = find_identifier($1);
    if (!ident) {
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is undeclared.", $2, $1);
        add_error($2, error_message);
    } else if (strcmp(ident->type, "output") == 0 && ident->assigned == 1) {
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already assigned.", $2, ident->name);
        add_error($2, error_message);
    } else if (strcmp(ident->type, "input") == 0) {
        char error_message[256];
        snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already assigned.", $2, $1);
        add_error($2, error_message);
    } else {
        ident->assigned = 1; // Mark as assigned
        ident -> value = result;
    }   
}
expression
;
 
expression : tNOT expression { $$ = !$2; result = $$;}
            | tLPR expression tRPR { $$ = $2;result = $$; }
           | tIDENTIFIER {
             Identifier* ident = find_identifier($1);
              if (!ident) {
                  char error_message[256];
                  snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is undeclared.", line_num, $1);
                  add_error(line_num, error_message);
              } else {
                  ident->used = 1; // Mark as used
                  $$ = ident -> value;
                  result = $$;
              }
           }
           | expression tAND expression { $$ = $1 && $3; result = $$;}
           | expression tOR expression {$$ = $1 || $3; result = $$;}
           | expression tXOR expression {$$ = $1 ^ $3; result = $$;}
           | tTRUE {$$ = 1; result = $$; }
           | tFALSE {$$ = 0; result = $$;}
;

//evaluation part

evaluations_block : evaluation 
            | evaluation evaluations_block
;


evaluation : tEVALUATE tIDENTIFIER tLPR EvaluationAssignmentList tRPR{
  identifier_table = reverse_list(identifier_table);
  Identifier *current = identifier_table;
  while(current){
    if(strcmp(current -> type, "input") == 0 && !current -> assigned){
      char error_message[256];
      snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is not assigned.", $1, current->name);
      add_error($1, error_message);
    }
    current = current -> next;
  }
  identifier_table = reverse_list(identifier_table);
  reset_assignments();

  char * circuit_name = $2;
  if(error_list == NULL){
    print_evaluations(circuit_name);
  }

  };

EvaluationAssignmentList : evaluationAssignment 
                         | evaluationAssignment tCOMMA EvaluationAssignmentList
;

evaluationAssignment : tIDENTIFIER tASSIGNMENT tTRUE {
  Identifier * ident = find_identifier($1);
  if(!ident){
    char error_message[256];
    snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is undeclared.", $2, $1);
    add_error($2, error_message);
  }
  else if(strcmp(ident -> type, "output") == 0 || strcmp(ident -> type, "node") == 0){
    char error_message[256];
    snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is not an input.", $2, $1);
    add_error($2, error_message);
  }
  else if(ident -> assigned == 1){
    char error_message[256];
    snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already assigned.", $2, $1);
    add_error($2, error_message);
  }
  else{
    ident -> assigned = 1;
    ident -> value = 1;
    
  }
}
| tIDENTIFIER tASSIGNMENT tFALSE {
    Identifier * ident = find_identifier($1);
    if(!ident){
      char error_message[256];
      snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is undeclared.", $2, $1);
      add_error($2, error_message);
    }
    else if(strcmp(ident -> type, "output") == 0 || strcmp(ident -> type, "node") == 0){
      char error_message[256];
      snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is not an input.", $2, $1);
      add_error($2, error_message);
    }
    else if(ident -> assigned == 1){
      char error_message[256];
      snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is already assigned.", $2, $1);
      add_error($2, error_message);
    }
    else{
      ident -> assigned = 1;
      ident -> value = 0;

    }
}
;

%%

void reset_assignments(){
  Identifier * current = identifier_table;
  while(current){
    if(strcmp(current -> type, "input") == 0 && current -> assigned == 1){
      current -> assigned = 0;
    }
  current = current -> next;
  }
}

Identifier * find_identifier(const char * name){
  Identifier *current = identifier_table;
  while(current){
    if(strcmp(current -> name, name) == 0){
      return current;
    }
    current = current -> next;
  }
  return NULL;
}


void declare_identifier(const char *name, char *type, int line) {
    // Add new identifier if it doesn't exist
    Identifier *new_identifier = (Identifier *)malloc(sizeof(Identifier));

    new_identifier->name = strdup(name);
    new_identifier->type = strdup(type);
    new_identifier->lineno = line;
    new_identifier->used = 0;
    new_identifier->assigned = 0;
    new_identifier->next = identifier_table;

    identifier_table = new_identifier;
}

void identifier_type(const char *type) {
    Identifier *current = identifier_table;
    while (current) {
        if (strcmp(current->type, "unknown") == 0) {
            free(current->type);
            current->type = strdup(type);
        }
        current = current->next;
    }
}

void unused_identifiers() {

    Identifier *current = identifier_table;
    while (current) {
      if ((strcmp(current->type, "input") == 0 || strcmp(current->type, "node") == 0) && !current->used) {
          char error_message[256];
          snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is not used.", current->lineno, current->name);
          add_error(current->lineno, error_message);
      }
      if ((strcmp(current->type, "node") == 0 || strcmp(current->type, "output") == 0) && !current->assigned) {
          char error_message[256];
          snprintf(error_message, sizeof(error_message), "ERROR at line %d: %s is not assigned.", current->lineno, current->name);
          add_error(current->lineno, error_message);
      }
      current = current->next;
    }

}

void add_error(int lineno, const char *message) {
    ErrorNode *new_error = (ErrorNode *)malloc(sizeof(ErrorNode));
    if (!new_error) {
        fprintf(stderr, "ERROR: Memory allocation failed while adding error.\n");
        exit(1);
    }
    new_error->lineno = lineno;
    new_error->appearance_order = current_error_order++;
    new_error->message = strdup(message);
    new_error->next = NULL;

    if (!error_list || (lineno < error_list->lineno) || 
       (lineno == error_list->lineno && new_error->appearance_order < error_list->appearance_order)) {
        new_error->next = error_list;
        error_list = new_error;
    } else {
        ErrorNode *current = error_list;
        while (current->next && 
              (current->next->lineno < lineno || 
              (current->next->lineno == lineno && current->next->appearance_order <= new_error->appearance_order))) {
            current = current->next;
        }
        new_error->next = current->next;
        current->next = new_error;
    }
}


void print_errors() {
    ErrorNode *current = error_list;
    while (current) {
        fprintf(stderr, "%s\n", current->message); 
        current = current->next;
    }
}

void free_errors() {
    ErrorNode *current = error_list;
    while (current) {
        ErrorNode *temp = current;
        current = current->next;
        free(temp->message);
        free(temp);
    }
    error_list = NULL;
}

Identifier* reverse_list(Identifier *head) {
    Identifier *prev = NULL, *current = head, *next = NULL;
    while (current) {
        next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    return prev;
}


void print_evaluations(const char *circuit_name) {

  printf("%s:", circuit_name);

  int first_output = 1;
  Identifier *current = identifier_table;

  while (current) {
      if (strcmp(current->type, "output") == 0) {
          if (!first_output) {
              printf(","); 
          }
          printf("%s=%s", current->name, current->value ? "true" : "false");
          first_output = 0;
      }
      current = current->next;
  }

  printf("\n");
}


int main () {
    if (yyparse()) {
        printf("ERROR\n");
        return 1;
    } else {
        unused_identifiers();
        print_errors(); 
        free_errors();
        return 0;
    }
}

