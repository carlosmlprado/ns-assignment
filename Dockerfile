FROM amazoncorretto:17-alpine3.17-jdk
MAINTAINER Carlos Prado
COPY target/ns-0.0.1-SNAPSHOT.jar ns-assignment-api.jar
ENTRYPOINT ["java", "-jar", "/ns-assignment-api.jar"]