#include "plane.hpp"

#include <random>

bool Plane::texture_loaded[2] = {false, false};
Texture Plane::textures[2]{};

void Plane::reset() {
    safe_mutex _{mtx};
    position    = {ScreenSize.x * px2m * 0.5, ScreenSize.y * px2m * 0.5};
    yaw         = 0_rad;
    roll        = 0_rad;
    target_roll = 0_rad;
    velocity    = {mps{0}, mps{-5}};
}

rad Plane::decide_roll() {
    safe_mutex _{mtx};
    auto speed = velocity.length();

    auto effective_speed = std::max(speed, mps{1.0});

    static constexpr auto T_danger  = 1.5_s;
    static constexpr auto T_warning = 3.0_s;

    auto danger_buffer  = effective_speed * T_danger;
    auto warning_buffer = effective_speed * T_warning;

    Vector2<m> geofence_min    = Geofence.position();
    Vector2<m> geofence_max    = Geofence.position() + Geofence.size();
    Vector2<m> geofence_center = Geofence.position() + Geofence.size() * 0.5;

    m dist_to_left   = position.x - geofence_min.x;
    m dist_to_right  = geofence_max.x - position.x;
    m dist_to_bottom = position.y - geofence_min.y;
    m dist_to_top    = geofence_max.y - position.y;

    m min_boundary_dist = std::min({dist_to_left, dist_to_right, dist_to_bottom, dist_to_top});

    Vector2<m> target_pos;
    double base_p_gain = 3.0;

    auto lookahead_position = position + velocity * T_danger;

    bool is_heading_out =
        !point_in_rect(lookahead_position, Geofence) &&
        min_boundary_dist < warning_buffer;

    if (is_heading_out || min_boundary_dist < danger_buffer) {
        // Go towards the center if it's in the danger zone
        target_pos  = geofence_center;
        base_p_gain = 5.0;
    } else if (min_boundary_dist < warning_buffer) {
        // If not in immediate danger, blend between waypoint and center
        double blend = ((min_boundary_dist - danger_buffer) / (warning_buffer - danger_buffer)).value;
        target_pos   = geofence_center * (1.0 - blend) + waypoint * blend;
        base_p_gain  = 3.5;
    } else {
        if (point_in_rect(waypoint, Geofence)) {
            target_pos = waypoint;
        } else {
            target_pos = geofence_center;
        }
        base_p_gain = 3.0;
    }

    Vector2<m> diff = target_pos - position;
    rad desired_yaw{std::atan2(diff.y.value, diff.x.value)};

    rad yaw_error = desired_yaw - yaw;
    while (yaw_error > rad{180_deg}) yaw_error -= rad{360_deg};
    while (yaw_error <= -rad{180_deg}) yaw_error += rad{360_deg};

    auto reference_speed = mps{20.0};

    double speed_compensation = (reference_speed / effective_speed).value;

    double final_gain = base_p_gain * std::min(speed_compensation, 2.0);

    rad final_command = yaw_error * final_gain;

    return std::clamp(final_command, -max_roll, max_roll);
}


void Plane::update(s dt) {
    safe_mutex _{mtx};
    target_roll = decide_roll();

    auto roll_difference = target_roll - roll;

    auto max_roll_step = roll_speed() * dt;

    if (abs(roll_difference) <= max_roll_step) {
        roll = target_roll;
    } else if (roll_difference > 0_rad) {
        roll += max_roll_step;
    } else {
        roll -= max_roll_step;
    }

    roll = clamp(roll, -max_roll, max_roll);

    auto yaw_change = roll * roll_yaw_factor * dt;
    yaw += yaw_change;

    yaw = fmod(yaw, rad{360_deg});
    if (yaw < 0_rad) yaw += rad{360_deg};

    auto forward_direction = Vector2{cos(yaw), sin(yaw)};

    auto thrust_acceleration = forward_direction * thrust_magnitude;

    auto drag_acceleration = -velocity * velocity.length().value * drag_factor;

    auto net_acceleration = thrust_acceleration + drag_acceleration + wind_acceleration();

    velocity += net_acceleration * dt;

    position += velocity * dt;

    if (!point_in_rect(position, Geofence)) {
        std::cout << "Crash: " << position << std::endl;
        reset();
    }
}

void Plane::draw(RenderTarget& window) {
    safe_mutex _{mtx};
    auto final_scale = sprite_initial_scale * 4.0f;

    stable_sprite.setPosition(pos_to_screen(position));
    stable_sprite.setRotation(sf::radians((-yaw + rad{90_deg}).value));
    stable_sprite.setScale(final_scale);
    window.draw(stable_sprite);

    moving_sprite.setPosition(stable_sprite.getPosition());
    moving_sprite.setRotation(stable_sprite.getRotation());
    float rollFactor = abs(sin(roll));
    moving_sprite.setScale(final_scale.componentWiseMul(sf::Vector2{1.f - rollFactor, 1.f}));
    window.draw(moving_sprite);

    CircleShape waypoint_marker;
    waypoint_marker.setRadius(5.f);
    waypoint_marker.setFillColor(sf::Color::Red);
    waypoint_marker.setOrigin({5.f, 5.f});
    waypoint_marker.setPosition(pos_to_screen(waypoint));
    window.draw(waypoint_marker);
}

Vector2<mps2> Plane::wind_acceleration() {
    static default_random_engine eng(42);
    static normal_distribution dist(0.0, 0.1);
    return Wind * dist(eng) * 3.33f;
}
