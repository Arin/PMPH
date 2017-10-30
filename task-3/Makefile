CXX        = nvcc

SOURCES_CPP =MatrixMult.cu
HELPERS     =MatrixMult.cu.h MatrixMult.h
EXECUTABLE  =MatrixMult


default: compile

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE)
$(EXECUTABLE): 
	$(CXX) -o $(EXECUTABLE) $(SOURCES_CPP)

run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE)
