package com.assignment.ns.jokes.controller;

import com.assignment.ns.jokes.JokesClient;
import com.assignment.ns.jokes.dto.response.JokeResponse;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class JokesControllerTest {

    @Autowired
    WebTestClient webTestClient;
    @SpyBean
    JokesClient jokeClient;

    @Test
    void test_getJoke_invokesJokeClient() {
        webTestClient.get()
                .uri("/api/v1/get-random-joke")
                .exchange()
                .expectBody(JokeResponse.class)
                .returnResult();

        verify(jokeClient, times(1)).getJokes();
    }

    @Test
    void test_getJoke_invokesJokesClientIsZero_WhenWrongURI() {
        webTestClient.get()
                .uri("/api/v1/get-random-jokee")
                .exchange()
                .expectBody(JokeResponse.class)
                .returnResult();

        verify(jokeClient, times(0)).getJokes();
    }

    @Test
    void test_getRandomJoke_returnRandomJoke() {
        var response = webTestClient.get()
                .uri("/api/v1/get-random-joke")
                .exchange()
                .expectBody(JokeResponse.class)
                .returnResult()
                .getResponseBody();

        assertNotNull(response);
    }

    @Test
    void test_getJoke_whenGet5xxResponse_thenThrowJokeInternalServerErrorException() {

        Mockito.when(jokeClient.getJokes()).thenThrow(WebClientResponseException.create(HttpStatus.INTERNAL_SERVER_ERROR.value(), "Bad Request", HttpHeaders.EMPTY, null, null));

        webTestClient.get()
                .uri("/api/v1/get-random-joke")
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .value(response -> {
                    Assertions.assertThat(response)
                            .isEqualTo("Unfortunately we couldn't retrieve your joke! Please try again later!");
                });

    }

    @Test
    void test_getJoke_whenGet4xxResponse_thenThrowJokeBadRequestException() {

        Mockito.when(jokeClient.getJokes()).thenThrow(WebClientResponseException.create(HttpStatus.BAD_REQUEST.value(), "Bad Request", HttpHeaders.EMPTY, null, null));

        webTestClient.get()
                .uri("/api/v1/get-random-joke")
                .exchange()
                .expectStatus().is4xxClientError()
                .expectBody(String.class)
                .value(response -> {
                    Assertions.assertThat(response)
                            .isEqualTo("Unfortunately we couldn't retrieve your joke! It's our fault! We are working on It! Please try again later!");
                });
    }
}