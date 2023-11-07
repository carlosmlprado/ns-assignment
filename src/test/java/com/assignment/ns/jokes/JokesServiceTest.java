package com.assignment.ns.jokes;

import com.assignment.ns.jokes.dto.request.Flag;
import com.assignment.ns.jokes.dto.request.Joke;
import com.assignment.ns.jokes.dto.request.JokesRequest;
import com.assignment.ns.jokes.exceptions.JokeInternalServerErrorException;
import com.assignment.ns.jokes.exceptions.JokeNotFoundException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.springframework.web.reactive.function.client.WebClientRequestException;

import static org.mockito.Mockito.*;

public class JokesServiceTest {

    JokesClient jokeClient;
    JokesService jokesService;

    @Before
    public void setUp() {
        jokeClient = Mockito.mock(JokesClient.class);
        jokesService = new JokesService(jokeClient);
    }

    @Test
    public void when_getRandomJoke_ThenReturn_SmallestOne_and_SafeNotExplicitNotSexist() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokes());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        var response = jokesService.getJoke();

        Assert.assertEquals(response.randomJoke(), "Oysters hate to give away their pearls because they are shellfish.");
    }

    @Test
    public void when_getRandomJoke_withNoSafeOne_ThenReturn_EmptyObject() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokesAndNoneIsSafe());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_withAllSexist_ThenThrow_JokeNotFoundException() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokesAndAllAreSexist());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_withAllExplicit_ThenThrow_JokeNotFoundException() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokesAndAllAreExplicit());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_and_ReturnEmptyList_thenThrow_JokeNotFoundException() {
        JokesRequest jokesRequest = new JokesRequest(new ArrayList<>());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_and_ReturnEmptyJokes_thenThrow_JokeNotFoundException() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithEmptyJokes());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_and_ReturnEqualLengthJokes_thenReturn_FirstOne() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokesWithSameLength());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertEquals(jokesService.getJoke().randomJoke(), "This is dummy 1.");
    }

    @Test
    public void when_getRandomJoke_withAllNotSafe_ThenThrow_JokeNotFoundException() {
        JokesRequest jokesRequest = new JokesRequest(buildDummyJokesListWithJokesAndAllNotSafe());
        when(jokeClient.getJokes()).thenReturn(jokesRequest);

        Assert.assertThrows(JokeNotFoundException.class, () -> jokesService.getJoke());
    }

    @Test
    public void when_getRandomJoke_withAllNotSafe_ThenThrow_JokeInternalServerErrorException() {
        when(jokeClient.getJokes()).thenThrow(WebClientRequestException.class);
        Assert.assertThrows(JokeInternalServerErrorException.class, () -> jokesService.getJoke());
    }

    private List<Joke> buildDummyJokesListWithEmptyJokes() {

        var joke1 = new Joke(null, 1, new Flag(false, false), false);
        var joke2 = new Joke(null, 2, new Flag(false, false), true);
        var joke3 = new Joke(null, 3, new Flag(false, false), true);
        var joke4 = new Joke(null, 4, new Flag(false, false), true);

        return Arrays.asList(joke1, joke2, joke3, joke4);
    }

    private List<Joke> buildDummyJokesListWithJokes() {

        var joke1 = new Joke("Oysters hate to give away their pearls because they are shellfish.", 1, new Flag(false, false), true);
        var joke2 = new Joke("My wife is really mad at the fact that I have no sense of direction. So I packed up my stuff and right.", 2, new Flag(false, false), true);
        var joke3 = new Joke("If you're here for the yodeling lesson, please form an orderly orderly orderly queue.", 3, new Flag(false, true), true);
        var joke4 = new Joke("I've got a really good UDP joke to tell you but I don’t know if you'll get it.", 4, new Flag(false, false), true);
        var joke5 = new Joke("I didn't vaccinate my 10 kids and the one that survived is fine!", 5, new Flag(true, false), false);

        return Arrays.asList(joke1, joke2, joke3, joke4, joke5);
    }

    private List<Joke> buildDummyJokesListWithJokesAndNoneIsSafe() {

        var joke1 = new Joke("Oysters hate to give away their pearls because they are shellfish.", 1, new Flag(false, false), false);
        var joke2 = new Joke("My wife is really mad at the fact that I have no sense of direction. So I packed up my stuff and right.", 2, new Flag(false, false), false);
        var joke3 = new Joke("If you're here for the yodeling lesson, please form an orderly orderly orderly queue.", 3, new Flag(false, true), false);

        return Arrays.asList(joke1, joke2, joke3);
    }

    private List<Joke> buildDummyJokesListWithJokesAndAllAreSexist() {

        var joke1 = new Joke("Oysters hate to give away their pearls because they are shellfish.", 1, new Flag(true, false), true);
        var joke2 = new Joke("My wife is really mad at the fact that I have no sense of direction. So I packed up my stuff and right.", 2, new Flag(true, false), true);
        var joke3 = new Joke("If you're here for the yodeling lesson, please form an orderly orderly orderly queue.", 3, new Flag(true, true), true);

        return Arrays.asList(joke1, joke2, joke3);
    }

    private List<Joke> buildDummyJokesListWithJokesAndAllAreExplicit() {

        var joke1 = new Joke("Oysters hate to give away their pearls because they are shellfish.", 1, new Flag(false, true), true);
        var joke2 = new Joke("My wife is really mad at the fact that I have no sense of direction. So I packed up my stuff and right.", 2, new Flag(false, true), true);
        var joke3 = new Joke("If you're here for the yodeling lesson, please form an orderly orderly orderly queue.", 3, new Flag(false, true), true);

        return Arrays.asList(joke1, joke2, joke3);
    }

    private List<Joke> buildDummyJokesListWithJokesWithSameLength() {

        var joke1 = new Joke("This is dummy 1.", 1, new Flag(false, false), true);
        var joke2 = new Joke("This is dummy 2.", 2, new Flag(false, false), true);
        var joke3 = new Joke("This is dummy 3.", 3, new Flag(false, false), true);

        return Arrays.asList(joke1, joke2, joke3);
    }

    private List<Joke> buildDummyJokesListWithJokesAndAllNotSafe() {

        var joke1 = new Joke("Oysters hate to give away their pearls because they are shellfish.", 1, new Flag(false, false), false);
        var joke2 = new Joke("My wife is really mad at the fact that I have no sense of direction. So I packed up my stuff and right.", 2, new Flag(false, false), false);
        var joke3 = new Joke("If you're here for the yodeling lesson, please form an orderly orderly orderly queue.", 3, new Flag(false, false), false);

        return Arrays.asList(joke1, joke2, joke3);
    }
}