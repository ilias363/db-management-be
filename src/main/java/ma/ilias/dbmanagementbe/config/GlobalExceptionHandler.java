package ma.ilias.dbmanagementbe.config;

import ma.ilias.dbmanagementbe.dto.ApiResponse;
import ma.ilias.dbmanagementbe.exception.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestController;

@ControllerAdvice
@RestController
public class GlobalExceptionHandler {
    @ExceptionHandler(MetadataNotFoundException.class)
    public ResponseEntity<ApiResponse<Void>> handleMetadataNotFoundException(MetadataNotFoundException e) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(e.getMessage())
                .success(false)
                .build(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(UnauthorizedActionException.class)
    public ResponseEntity<ApiResponse<Void>> handleUnauthorizedActionException(UnauthorizedActionException ex) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(ex.getMessage())
                .success(false)
                .build(), HttpStatus.UNAUTHORIZED);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<?> handleValidationExceptions(MethodArgumentNotValidException ex) {
        StringBuilder errorMessage = new StringBuilder();
        for (FieldError error : ex.getBindingResult().getFieldErrors()) {
            errorMessage.append(error.getField()).append(": ").append(error.getDefaultMessage()).append("\n");
        }
        for (ObjectError error : ex.getBindingResult().getGlobalErrors()) {
            errorMessage.append("global: ").append(error.getDefaultMessage()).append("\n");
        }
        if (!errorMessage.isEmpty()) {
            errorMessage.deleteCharAt(errorMessage.length() - 1);
        }
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(errorMessage.toString())
                .success(false)
                .build(), HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(EntityNotFoundException.class)
    public ResponseEntity<ApiResponse<Void>> handleEntityNotFoundException(EntityNotFoundException ex) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(ex.getMessage())
                .success(false)
                .build(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(RecordNotFoundException.class)
    public ResponseEntity<ApiResponse<Void>> handleRecordNotFoundException(RecordNotFoundException ex) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(ex.getMessage())
                .success(false)
                .build(), HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(InvalidRecordDataException.class)
    public ResponseEntity<ApiResponse<Void>> handleInvalidRecordDataException(InvalidRecordDataException ex) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message(ex.getMessage())
                .success(false)
                .build(), HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ApiResponse<Void>> handleGenericException(Exception ex) {
        return new ResponseEntity<>(ApiResponse.<Void>builder()
                .message("An error occurred: " + ex.getMessage())
                .success(false)
                .build(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}