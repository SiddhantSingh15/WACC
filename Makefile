all:
	sbt assembly

test:
	sbt test

clean:
	rm -rf bin target