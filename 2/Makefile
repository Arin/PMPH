CXX        = nvcc

<<<<<<< HEAD
SOURCES_CPP =SquareFunct.cu
HELPERS     =Timer.h TransMainGen.h
EXECUTABLE  =SquareFunct
=======
SOURCES_CPP =ScanMainGen.cu
HELPERS     =ScanHost.cu.h ScanKernels.cu.h
EXECUTABLE  =sgmScanInc
>>>>>>> 9a28a0bab6a846eeda11140c2cfc1ee418c8968a


default: compile

.cu.o: $(SOURCES_CPP) $(HELPERS)
	$(CXX) -c $@ $<


compile: $(EXECUTABLE)
$(EXECUTABLE): 
	$(CXX) -o $(EXECUTABLE) $(SOURCES_CPP)

<<<<<<< HEAD
=======

>>>>>>> 9a28a0bab6a846eeda11140c2cfc1ee418c8968a
run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE)
<<<<<<< HEAD
=======


>>>>>>> 9a28a0bab6a846eeda11140c2cfc1ee418c8968a
