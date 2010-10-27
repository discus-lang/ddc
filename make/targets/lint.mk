# Linting

.PHONY	: hlint
hlint	:
	@echo "* Running HLint"
	hlint 	\
		-i "Use camelCase" 	\
		-i "Use ."		\
		-i "Use :"		\
		-i "Use >>="		\
		-i "Eta reduce"		\
		src

