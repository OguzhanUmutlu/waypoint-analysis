#pragma once

#include "main.hpp"

constexpr Vector2 Wind{mps2{15}, mps2{0}};
static Rect Geofence{0_m, 0_m, ScreenSize.x * px2m, ScreenSize.y * px2m};

struct Plane
{
    Sprite stable_sprite;
    Sprite moving_sprite;
    Vector2<m> position{};
    rad yaw{};
    rad roll{};
    rad target_roll{};
    Vector2<mps> velocity{};
    Vector2<m> waypoint{ScreenSize.x * px2m * 0.5, ScreenSize.y * px2m * 0.5};

    static constexpr rad aileron_deflection_angle{2_deg};
    static constexpr rad control_power{0.2_deg};
    static constexpr rad roll_damping{0.3_deg};
    static constexpr auto wing_span = 1.5_m;
    static constexpr rad max_roll{30_deg};
    static constexpr auto roll_yaw_factor = 1.5 / 1_s;
    static constexpr auto thrust_magnitude = 10.0_m / 1_s / 1_s;
    static constexpr auto drag_factor = thrust_magnitude / (mps{10} * mps{10}) * mps{1};

    const sf::Vector2<float> sprite_initial_scale{
        static_cast<float>((wing_span * m2px / stable_sprite.getLocalBounds().size.x).value),
        static_cast<float>((wing_span * m2px / stable_sprite.getLocalBounds().size.y).value)
    };

    rad_ps roll_speed() const
    {
        auto cruise_velocity = (velocity - Wind * 1_s).length(); // m/s

        // p_ss
        return clamp(
            abs(2 * cruise_velocity / wing_span * (control_power / -roll_damping) * aileron_deflection_angle),
            rad_ps{0}, rad_ps{1}
        );
    }

    Plane() : stable_sprite(loadTexture(0, "plane_stable.png")),
              moving_sprite(loadTexture(1, "plane_moving.png"))
    {
        auto stable_bound = stable_sprite.getLocalBounds();
        auto moving_bound = moving_sprite.getLocalBounds();
        if (stable_bound != moving_bound)
        {
            std::cout << stable_bound.size.x << std::endl;
            std::cout << stable_bound.size.y << std::endl;
            std::cout << moving_bound.size.x << std::endl;
            std::cout << moving_bound.size.y << std::endl;
            throw runtime_error("Plane textures have different sizes");
        }
        stable_sprite.setOrigin(stable_bound.size / 2.0f);
        moving_sprite.setOrigin(moving_bound.size / 2.0f);
        reset();
    }

    void reset();
    rad decide_roll(s _dt) const;
    void update(s dt);
    void draw(RenderTarget& window);

    static Vector2<mps2> wind_acceleration();

private:
    static bool texture_loaded[2];
    static Texture textures[2];

    static Texture& loadTexture(int index, std::string_view path)
    {
        if (!texture_loaded[index])
        {
            if (!textures[index].loadFromFile(path))
            {
                throw runtime_error("Failed to load plane texture");
            }
            texture_loaded[index] = true;
        }
        return textures[index];
    }
};
