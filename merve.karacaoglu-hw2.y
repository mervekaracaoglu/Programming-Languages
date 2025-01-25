%{
#include <stdio.h>
int yylex();

int error = 0;
void yyerror (const char *s) 
{
    error = 1;
}

%}

%token tINPUT tOUTPUT tNODE tEVALUATE tAND tOR tXOR tNOT tTRUE tFALSE tLPR tRPR tASSIGNMENT tCOMMA tIDENTIFIER tBLANK

%left tOR tXOR
%left tAND
%left tNOT

%%

lcd_program: 
            | all_blocks
;

all_blocks : declaration_block circuit_design_block evaluation_block
;

declaration : tINPUT ident_list
            | tNODE ident_list
            | tOUTPUT ident_list
;

declaration_block : declaration
            | declaration_block declaration
;


ident_list: tIDENTIFIER
            | ident_list tCOMMA tIDENTIFIER
;

expr : tIDENTIFIER
    | tNOT expr
    | expr tAND expr
    | expr tOR expr
    | expr tXOR expr
    | tTRUE
    | tFALSE
    | tLPR expr tRPR
;

circuit_design : tIDENTIFIER tASSIGNMENT expr
;

circuit_design_block: circuit_design
                    | circuit_design_block circuit_design
;


input_eval : tIDENTIFIER tASSIGNMENT tTRUE
            | tIDENTIFIER tASSIGNMENT tFALSE
;


input_eval_list : input_eval
                | input_eval_list tCOMMA input_eval
;

evaluation: tEVALUATE tIDENTIFIER tLPR input_eval_list tRPR
;

evaluation_block: evaluation
                | evaluation_block evaluation
;

            

%%

int main ()
 {
    if (yyparse())
    {
    printf("ERROR\n");
    return 1;
    }
    else
    {
    printf("OK\n");
    return 0;
    }
 }