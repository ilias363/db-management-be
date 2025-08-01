package ma.ilias.dbmanagementbe.service;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ma.ilias.dbmanagementbe.dao.repositories.RefreshTokenRepository;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
@Slf4j
public class RefreshTokenCleanupService {

    private final RefreshTokenRepository refreshTokenRepository;

    @Scheduled(fixedRate = 60000)
    @Transactional
    public void deleteExpiredTokens() {
        refreshTokenRepository.deleteExpiredTokens(LocalDateTime.now());
        log.info("RefreshTokenCleanupService delete expired tokens");
    }
}
