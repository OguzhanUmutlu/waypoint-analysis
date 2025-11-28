#include "main.hpp"

#include <optional>
#include "plane.hpp"

constexpr auto TargetFPS = 144_Hz;
constexpr auto TargetUPS = 300_Hz;

int main() {
    RenderWindow window(VideoMode({ScreenSize.x.value, ScreenSize.y.value}), "2D Fixed-Wing Simulation");

    Plane fixed_wing;

    auto last = get_time_s();
    s render_accumulator{};
    s update_accumulator{};

    while (window.isOpen()) {
        while (true) {
            auto ev = window.pollEvent();
            if (!ev.has_value()) break;
            Event event = ev.value();

            if (event.is<Event::Closed>()) window.close();
            if (event.is<Event::MouseMoved>()) {
                const auto& m_event = event.getIf<Event::MouseMoved>();

                auto mouse_pos = screen_to_pos(m_event->position);

                if (mouse_pos.has_value()) {
                    fixed_wing.waypoint = mouse_pos.value();
                }
            }
        }

        if (!window.hasFocus()) {
            last = get_time_s();
            continue;
        }

        if (!window.isOpen()) break;

        auto now = get_time_s();
        auto dt  = now - last;
        if (dt == 0_s) continue;

        render_accumulator += dt;
        update_accumulator += dt;
        last = now;

        if (update_accumulator < s{1.0 / TargetUPS}) continue;
        fixed_wing.update(s{1 / TargetUPS});
        update_accumulator = 0.0_s;

        if (render_accumulator < s{1.0 / TargetFPS}) continue;
        render_accumulator = 0.0_s;
        window.clear(Color::White);
        fixed_wing.draw(window);
        window.display();
        window.setTitle("2D Fixed-Wing Simulation");
    }
}
