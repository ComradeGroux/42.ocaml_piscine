BOLD	:= \033[1m
GRAY	:= \033[90m
GREEN	:= \033[32m
BLUE	:= \033[34m
RESET	:= \033[0m
ERASE	:= \r\033[2K

NAME = a.out

CAMLOPT = ocamlopt
CAMLC = ocamlc
CAMLDEP = ocamldep
DEP_FILE = $(BUILD_DIR)/.depend

SRCS_DIR = src
BUILD_DIR = build
OPT_DIR = opt
BYT_DIR = byt

SRCS = $(wildcard $(SRCS_DIR)/*.ml)
OPTOBJS = $(patsubst $(SRCS_DIR)/%.ml, $(BUILD_DIR)/$(OPT_DIR)/%.cmx, $(SRCS))
BYTOBJS = $(patsubst $(SRCS_DIR)/%.ml, $(BUILD_DIR)/$(BYT_DIR)/%.cmo, $(SRCS))

FLAGS = -I $(BUILD_DIR)

all: $(BUILD_DIR) $(DEP_FILE) opt byt $(NAME)

$(DEP_FILE): $(SRCS) | $(BUILD_DIR)
	@$(CAMLDEP) -I $(SRCS_DIR) $(SRCS) | sed 's|$(SRCS_DIR)/\([^ ]*\)\.cmo|$(BUILD_DIR)/\1.cmo|g; s|$(SRCS_DIR)/\([^ ]*\)\.cmx|$(BUILD_DIR)/\1.cmx|g; s|$(SRCS_DIR)/\([^ ]*\)\.cmi|$(BUILD_DIR)/\1.cmi|g' > $(DEP_FILE)

-include $(DEP_FILE)

$(NAME):
	@[ -L $(NAME) ] || \
	 (printf "\n$(BOLD)$(BLUE)Creation of a symbolic link$(RESET)\n" && \
	  printf "$(GRAY)  Creating symlink $(NAME) -> $(NAME).opt...$(RESET)" && \
	  ln -s $(NAME).opt $(NAME) && \
	  printf "$(ERASE)$(GREEN)  ✓ $(NAME) -> $(NAME).opt$(RESET)\n")

opt: $(NAME).opt
byt: $(NAME).byt

$(BUILD_DIR)/.opt_start: $(SRCS) | $(BUILD_DIR)
	@printf "$(BOLD)$(BLUE)Compiling $(NAME).opt$(RESET)\n"
	@touch $@

$(BUILD_DIR)/.byt_start: $(SRCS) | $(BUILD_DIR)
	@printf "\n$(BOLD)$(BLUE)Compiling $(NAME).byt$(RESET)\n"
	@touch $@

$(NAME).opt: $(OPTOBJS)
	@printf "$(BOLD)Linking $(NAME).opt$(RESET)\n"
	@$(CAMLOPT) $(FLAGS) -o $@ $^
	@printf "$(GREEN)  ✓ $(NAME).opt ready$(RESET)\n"

$(BUILD_DIR)/$(OPT_DIR)/%.cmx: $(SRCS_DIR)/%.ml | $(BUILD_DIR)/.opt_start
	@printf "$(GRAY)  $<...$(RESET)" && \
	 $(CAMLOPT) $(FLAGS) -c -o $@ $< && \
	 printf "$(ERASE)$(GREEN)  ✓ $<$(RESET)\n"

$(NAME).byt: $(BYTOBJS)
	@printf "$(BOLD)Linking $(NAME).byt$(RESET)\n"
	@$(CAMLC) $(FLAGS) -o $@ $^
	@printf "$(GREEN)  ✓ $(NAME).byt ready$(RESET)\n"

$(BUILD_DIR)/$(BYT_DIR)/%.cmo: $(SRCS_DIR)/%.ml | $(BUILD_DIR)/.byt_start
	@printf "$(GRAY)  $<...$(RESET)" && \
	 $(CAMLC) $(FLAGS) -c -o $@ $< && \
	 printf "$(ERASE)$(GREEN)  ✓ $<$(RESET)\n"

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(BUILD_DIR)/$(OPT_DIR)
	@mkdir -p $(BUILD_DIR)/$(BYT_DIR)

clean:
	@printf "$(BOLD)$(BLUE)Cleaning $(NAME) objects...$(RESET)\n"
	@printf "$(GRAY)  Removing $(BUILD_DIR)...$(RESET)" && \
	 rm -rf $(BUILD_DIR) && \
	 printf "$(ERASE)"
	@printf "$(GREEN)  ✓ Build files cleaned$(RESET)\n"

fclean: clean
	@printf "$(GRAY)  Removing $(NAME)...$(RESET)" && \
	 rm -f $(NAME) && \
	 printf "$(ERASE)"
	@printf "$(GREEN)  ✓ $(NAME) cleaned$(RESET)\n"
	@printf "$(GRAY)  Removing $(NAME).opt...$(RESET)" && \
	 rm -f $(NAME).opt && \
	 printf "$(ERASE)"
	@printf "$(GREEN)  ✓ $(NAME).opt cleaned$(RESET)\n"
	@printf "$(GRAY)  Removing $(NAME).byt...$(RESET)" && \
	 rm -f $(NAME).byt && \
	 printf "$(ERASE)"
	@printf "$(GREEN)  ✓ $(NAME).byt cleaned$(RESET)\n"

re: fclean all

.PHONY: all clean fclean re opt byt
