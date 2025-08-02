package ma.ilias.dbmanagementbe.dto.auth;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TokenRefreshResponseDto {
    private String newAccessToken;
    private String newRefreshToken;
    private Long newAccessTokenExpiry;
    private Long newRefreshTokenExpiry;
}
