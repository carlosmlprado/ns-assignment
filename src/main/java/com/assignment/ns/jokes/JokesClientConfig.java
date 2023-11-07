package com.assignment.ns.jokes;

import com.assignment.ns.jokes.exceptions.JokeInternalServerErrorException;
import com.assignment.ns.jokes.exceptions.JokeNotFoundException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.support.WebClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;
import reactor.core.publisher.Mono;

@Configuration
public class JokesClientConfig {

    @Bean
    public JokesClient jokeClient(JokesProperties jokeProperties) {
        WebClient webClient = WebClient.builder()
                .baseUrl(jokeProperties.getUri())
                .defaultStatusHandler(
                        httpStatusCode -> HttpStatus.NOT_FOUND == httpStatusCode,
                        response -> Mono.error(new JokeNotFoundException()))
                .defaultStatusHandler(
                        HttpStatusCode::is5xxServerError,
                        response -> Mono.error(new JokeInternalServerErrorException()))
                .build();

        return HttpServiceProxyFactory
                .builder(WebClientAdapter.forClient(webClient))
                .build()
                .createClient(JokesClient.class);
    }
}
