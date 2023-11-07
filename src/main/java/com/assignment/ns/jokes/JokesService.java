package com.assignment.ns.jokes;

import com.assignment.ns.jokes.dto.request.Joke;
import com.assignment.ns.jokes.dto.request.JokesRequest;
import com.assignment.ns.jokes.dto.response.JokeResponse;
import com.assignment.ns.jokes.exceptions.JokeBadRequestException;
import com.assignment.ns.jokes.exceptions.JokeInternalServerErrorException;
import com.assignment.ns.jokes.exceptions.JokeNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.*;
import java.util.function.Predicate;

@Slf4j
@Service
@RequiredArgsConstructor
public class JokesService {
    private final JokesClient jokeClient;

    public JokeResponse getJoke() {
        JokesRequest jokeResponse;

        try {
            log.info("Calling jokes api...");
            jokeResponse = jokeClient.getJokes();

        } catch (WebClientRequestException e) {
            log.error("Error calling jokes api due server error: {}", e.getMessage());
            throw new JokeInternalServerErrorException();
        } catch (WebClientResponseException e) {
            log.error("Error calling jokes api due client error: {}", e.getMessage());
            throw new JokeBadRequestException();
        }

        if (ObjectUtils.isEmpty(jokeResponse.jokes())) {
            throw new JokeNotFoundException();
        }

        log.debug("Response from jokes api: {}", jokeResponse.jokes());
        return filterResponse(jokeResponse.jokes());
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
        Predicate<Joke> predicate = j -> Boolean.FALSE.equals(j.flags().explicit())
                && Boolean.FALSE.equals(j.flags().sexist()) && Boolean.TRUE.equals(j.safe() && !ObjectUtils.isEmpty(j.joke()));
        return predicate;
    }
}
