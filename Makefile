NUM_THREADS=24

CMAKE_OUTPUT_DIR=target
CMAKE_BUILD_DIR=$(CMAKE_OUTPUT_DIR)/build

DONLANG_COMPILER=$(CMAKE_BUILD_DIR)/donlang
DONLANG_COMPILE_FLAGS=-p

DONLANG_SOURCE=main.don
DONLANG_LL_OUT = $(CMAKE_BUILD_DIR)/$(DONLANG_SOURCE:.don=.ll)
DONLANG_EXECUTABLE = $(CMAKE_BUILD_DIR)/$(DONLANG_SOURCE:.don=.out)

# silence the the output when changing directories
MAKEFLAGS += --no-print-directory

all: build

cmake:
	mkdir -p $(CMAKE_OUTPUT_DIR)
	mkdir -p $(CMAKE_BUILD_DIR)
	cmake -S . -B $(CMAKE_OUTPUT_DIR) -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
	mv $(CMAKE_OUTPUT_DIR)/compile_commands.json .


build:
	cmake --build $(CMAKE_OUTPUT_DIR) -j$(NUM_THREADS)

don:
	$(DONLANG_COMPILER) $(DONLANG_COMPILE_FLAGS) -o $(DONLANG_LL_OUT) $(DONLANG_SOURCE)
	clang $(DONLANG_LL_OUT) -lm -o $(DONLANG_EXECUTABLE)

don-reload: don don-run

don-run:
	$(DONLANG_EXECUTABLE)


clean:
	rm -rf $(CMAKE_OUTPUT_DIR)
