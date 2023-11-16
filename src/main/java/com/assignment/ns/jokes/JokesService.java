package com.assignment.ns.jokes;

import com.assignment.ns.jokes.dto.request.Joke;
import com.assignment.ns.jokes.dto.request.JokesRequest;
import com.assignment.ns.jokes.dto.response.JokeResponse;
import com.assignment.ns.jokes.exceptions.JokeBadRequestException;
import com.assignment.ns.jokes.exceptions.JokeInternalServerErrorException;
import com.assignment.ns.jokes.exceptions.JokeNotFoundException;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.*;
import java.util.function.Predicate;

@Slf4j
@Service
@RequiredArgsConstructor
public class JokesService {
    private final JokesClient jokeClient;

    @CircuitBreaker(name = "callbackRandomJoke", fallbackMethod = "fallbackRandomJoke")
    public JokeResponse getJoke() {
        ResponseEntity<JokesRequest> jokeResponse;

        log.info("Calling jokes api...");
        jokeResponse = jokeClient.getJokes();

        if (jokeResponse.getStatusCode().is4xxClientError()) {
            throw new JokeBadRequestException();
        } else if (jokeResponse.getStatusCode().is5xxServerError()) {
            throw new JokeInternalServerErrorException();
        }

        if (!ObjectUtils.isEmpty(jokeResponse.getBody()) && !ObjectUtils.isEmpty(jokeResponse.getBody().jokes())) {
            log.debug("Response from jokes api: {}", jokeResponse.getBody().jokes());
            return filterResponse(jokeResponse.getBody().jokes());
        } else {
            throw new JokeNotFoundException();
        }
    }

    private JokeResponse filterResponse(List<Joke> list) {

        Predicate<Joke> predicate = buildPredicateToFilterJokes();

        log.info("Filtering jokes by not sexist/explicit, safe and with the smallest length");
        Optional<Joke> joke = list.stream()
                .filter(predicate)
                .min(Comparator.comparing(obj -> obj.joke().length()));

        log.debug("Joke Response: {}", joke);
        return joke.map(value -> new JokeResponse(value.id(), value.joke())).orElseThrow(JokeNotFoundException::new);
    }

    private Predicate<Joke> buildPredicateToFilterJokes() {
        return j -> Boolean.FALSE.equals(j.flags().explicit())
                && Boolean.FALSE.equals(j.flags().sexist()) && Boolean.TRUE.equals(j.safe()) && !ObjectUtils.isEmpty(j.joke());
    }

    @SuppressWarnings("unused")
    private JokeResponse fallbackRandomJoke(Throwable e) {
        log.error("Error calling jokes api: {}", e.getMessage());
        throw new JokeInternalServerErrorException();
    }
}
