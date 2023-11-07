package com.assignment.ns.jokes;

import com.assignment.ns.jokes.dto.response.JokeResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1")
public class JokesController {
    private final JokesService jokesService;

    @Operation(summary = "Get your no explicit/sexist Joke!")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Joke Retrieved!"),
            @ApiResponse(responseCode = "400", description = "Unfortunately we couldn't retrieve your joke! It's our fault! We are working on It! Please try again later!"),
            @ApiResponse(responseCode = "404", description = "Joke Not Found!"),
            @ApiResponse(responseCode = "500", description = "Unfortunately we couldn't retrieve your joke! Please try again later!")})
    @GetMapping("/get-random-joke")
    public ResponseEntity<JokeResponse> getRandomJoke() {
        return ResponseEntity.ok(jokesService.getJoke());
    }
}
