Assignment for NS Java developer on November/23.

The goal of the api is communicating with a third party api which retrieves some jokes and once received, 
the api will filter It by some conditions:
- safe == true;
- sexist == false;
- explicit == false;
- shortest joke.

The application has one properties file for each environment (Test, Acceptance and Production). Since we don't have 
much different configurations, the only thing that differs one from another is the server port where they run.

APi was made with Spring Framework and Java 17.
It runs with docker and API doc is made with Open API.
There are 3 properties environments, which are tst, acc and prd.
You can change between them by adding the command below in VM Options:
[-Dspring.profiles.active={environment}]
The default is tst.

Swagger API documentation AFTER RUNNING THE APPLICATION: [http://localhost:8060/swagger-ui.html#/]

We can also check status of application with actuator endpoints:
[http://localhost:8060/actuator/health]
[http://localhost:8060/actuator/env]
[http://localhost:8060/actuator/metrics]

Contains 1 Controller, which is:

JokeController: with entry point "/api/v1"
With the methods:

@GetMapping("/get-random-joke")
Get a random joke.

To run it, You need maven and Java 17 installed at your machine.
- Clone this repo to your machine
- Via command line, use 'cd' command to go the project's folder;
- Execute the command [mvn clean install] - It will generate the jar of the application;
- Execute the command [java -jar target/ns-0.0.1-SNAPSHOT.jar] to run the application.

- You can use Postman to interact with the api: [http://localhost:8060/api/v1/get-random-joke]

- Documentation was made with OPEN API and can be accessed through the link: [http://localhost:8060/swagger-ui/index.html#/] (You can also interact with the api here).

Pay attention in which environment you are running to change the port.

- And also you can run via Docker.
- Use 'cd' command to go to the folder of the project and run the command:
- Execute: [mvn clean install]

    - build docker file
      [docker build -t ns-assignment-api .]
    - run dockerfile
      [docker run -dp8060:8060 ns-assignment-api] (tst environment)
      [docker run -e "SPRING_PROFILES_ACTIVE=acc" -dp8070:8070 ns-assignment-api] (acc environment)
      [docker run -e "SPRING_PROFILES_ACTIVE=prd" -dp8090:8090 ns-assignment-api] (prd environment)