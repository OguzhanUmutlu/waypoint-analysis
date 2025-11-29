#pragma once
#include <iostream>
#include <mutex>
#include <SFML/Graphics.hpp>
#include "Unit.hpp"
#include "Vector2.hpp"
#include "Rect.hpp"
using namespace std;
using namespace Unit::defaults;
using Texture      = sf::Texture;
using Sprite       = sf::Sprite;
using CircleShape  = sf::CircleShape;
using RenderWindow = sf::RenderWindow;
using RenderTarget = sf::RenderTarget;
using VideoMode    = sf::VideoMode;
using Event        = sf::Event;
using Color        = sf::Color;
using MouseButton  = sf::Mouse::Button;

using mps    = decltype(m{} / s{});
using mps2   = decltype(m{} / s{} / s{});
using rad_ps = decltype(rad{} / s{});
using deg_ps = decltype(deg{} / s{});

static auto get_time() {
    return s{
        static_cast<double>(chrono::high_resolution_clock::now().time_since_epoch() /
            chrono::microseconds(1)) / 1e6f
    };
}

template <typename... Args>
static void println(Args&&... args) {
    (cout << ... << args) << endl;
}

template <typename T>
static bool point_in_rect(Vector2<T> vec, Rect<T> rect) {
    return vec.x >= rect.x && vec.x <= rect.x + rect.width &&
        vec.y >= rect.y && vec.y <= rect.y + rect.height;
}

static Vector2 ScreenSize{800_px, 800_px};

constexpr auto _m2px_constant = 5.0_px;

constexpr auto m2px = _m2px_constant / 1_m;
constexpr auto px2m = 1_m / _m2px_constant;

static constexpr auto pos_to_screen(const Vector2<m>& vec) {
    return sf::Vector2{
        static_cast<float>((vec.x * m2px).value),
        static_cast<float>((ScreenSize.y - vec.y * m2px).value)
    };
}

template <typename T>
static constexpr std::optional<Vector2<m>> screen_to_pos(const sf::Vector2<T>& vec) {
    if (vec.x < 0 || vec.y < 0 ||
        vec.x >= static_cast<T>(ScreenSize.x.value) ||
        vec.y >= static_cast<T>(ScreenSize.y.value)) {
        return nullopt;
    }
    return Vector2{
        px{static_cast<unsigned>(vec.x)} * px2m,
        (ScreenSize.y - px{static_cast<unsigned>(vec.y)}) * px2m
    };
}
