#pragma once
#include <mutex>

struct safe_mutex {
    std::mutex& mtx;
    bool locked{false};

    explicit safe_mutex(std::mutex& m) : mtx(m) {
        locked = mtx.try_lock();
    }

    ~safe_mutex() {
        if (locked) mtx.unlock();
    }
};
