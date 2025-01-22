##
## EPITECH PROJECT, 2023
## B-FUN-500-COT-5-2-glados-aman.menda [WSL: Ubuntu-22.04]
## File description:
## Makefile
##

RM	=	rm -f

STACK	:=	$(shell stack path --local-install-root)

all: glados psc

glados:
	stack build
	cp $(STACK)/bin/glados-exe ./glados

psc:
	stack build
	cp $(STACK)/bin/psc-exe ./psc

clean:
	stack clean

fclean:	clean
	$(RM) generator.out
	$(RM) glados
	$(RM) psc

tests_run:
	stack test --coverage

re: fclean all

.PHONY:	all clean fclean re
