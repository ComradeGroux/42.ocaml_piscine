NAME = a.out

CAMLOPT = ocamlopt
CAMLC = ocamlc
CAMLDEP = ocamldep
DEP_FILE = $(BUILD_DIR)/.depend

SRCS_DIR = src
BUILD_DIR = build

SRCS = $(wildcard $(SRCS_DIR)/*.ml)
OPTOBJS = $(patsubst $(SRCS_DIR)/%.ml, $(BUILD_DIR)/%.cmx, $(SRCS))
OBJS = $(patsubst $(SRCS_DIR)/%.ml, $(BUILD_DIR)/%.cmo, $(SRCS))

FLAGS = -I $(BUILD_DIR)

all: $(BUILD_DIR) $(DEP_FILE) $(NAME) opt byt

$(DEP_FILE): $(SRCS) | $(BUILD_DIR)
	$(CAMLDEP) -I $(SRCS_DIR) $(SRCS) | sed 's|$(SRCS_DIR)/\([^ ]*\)\.cmo|$(BUILD_DIR)/\1.cmo|g; s|$(SRCS_DIR)/\([^ ]*\)\.cmx|$(BUILD_DIR)/\1.cmx|g; s|$(SRCS_DIR)/\([^ ]*\)\.cmi|$(BUILD_DIR)/\1.cmi|g' > $(DEP_FILE)

-include $(DEP_FILE)

$(NAME):
	[ -L $(NAME) ] || ln -s $(NAME).opt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) $(FLAGS) -o $@ $^
$(BUILD_DIR)/%.cmx: $(SRCS_DIR)/%.ml | $(BUILD_DIR)
	$(CAMLOPT) $(FLAGS) -c -o $@ $<


$(NAME).byt: $(OBJS)
	$(CAMLC) $(FLAGS) -o $@ $^
$(BUILD_DIR)/%.cmo: $(SRCS_DIR)/%.ml | $(BUILD_DIR)
	$(CAMLC) $(FLAGS) -c -o $@ $<

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)




clean:
	rm -rf $(BUILD_DIR)

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

re: fclean all

.PHONY: all clean fclean re opt byt
