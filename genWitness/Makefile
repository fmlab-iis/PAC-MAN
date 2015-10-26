TARGET = genWitness
SOURCE = nodes.cpp main.cpp utils.cpp
CXXFLAGS += -Wall -Wno-unused-but-set-variable -g -std=c++11 
all:
	g++ $(CXXFLAGS) -o $(TARGET) $(SOURCE)
debug:
	g++ -D DEBUG $(CXXFLAGS) -o $(TARGET) $(SOURCE)
clean:
	rm -f $(TARGET) 
