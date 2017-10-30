CXX        = nvcc

SOURCES_CPP =SquareFunct.cu
HELPERS     =Timer.h TransMainGen.h
EXECUTABLE  =SquareFunct


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
