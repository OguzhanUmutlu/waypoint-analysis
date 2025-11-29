#include "main.hpp"

#include <optional>
#include "plane.hpp"

constexpr auto TargetFPS = 144_Hz;
constexpr auto TargetUPS = 300_Hz;


int main() {
    RenderWindow window(VideoMode({ScreenSize.x.value, ScreenSize.y.value}), "2D Fixed-Wing Simulation");

    auto last = get_time();
    s render_accumulator{};
    bool mouse_down = false;

    std::atomic is_running = true;

    auto fixed_wing = std::make_shared<Plane>();

    std::thread thread([fixed_wing, &is_running]() {
        auto t_last = get_time();
        while (is_running.load()) {
            auto t_now = get_time();
            auto dt    = t_now - t_last;

            t_last = t_now;
            fixed_wing->update(dt);
            sf::sleep(sf::seconds((1.0_s / TargetUPS).value));
        }
    });

    while (window.isOpen()) {
        while (true) {
            auto ev = window.pollEvent();
            if (!ev.has_value()) break;
            Event event = ev.value();

            if (event.is<Event::Closed>()) window.close();
            if (event.is<Event::MouseButtonPressed>()) {
                const auto& mb_event = event.getIf<Event::MouseButtonPressed>();
                if (mb_event->button == MouseButton::Left) mouse_down = true;
            }
            if (event.is<Event::MouseButtonReleased>()) {
                const auto& mb_event = event.getIf<Event::MouseButtonReleased>();
                if (mb_event->button == MouseButton::Left) mouse_down = false;
            }
            if (event.is<Event::MouseMoved>() && mouse_down) {
                const auto& m_event = event.getIf<Event::MouseMoved>();

                auto mouse_pos = screen_to_pos(m_event->position);

                if (mouse_pos.has_value()) {
                    fixed_wing->set_waypoint(mouse_pos.value());
                }
            }
        }

        if (!window.hasFocus()) {
            last = get_time();
            continue;
        }

        if (!window.isOpen()) break;

        auto now = get_time();
        auto dt  = now - last;
        if (dt == 0_s) continue;

        render_accumulator += dt;
        last = now;

        if (render_accumulator < s{1.0 / TargetFPS}) continue;
        render_accumulator = 0.0_s;
        window.clear(Color::White);
        fixed_wing->draw(window);
        window.display();
        window.setTitle("2D Fixed-Wing Simulation");
    }

    is_running.store(false);

    if (thread.joinable()) thread.join();
}
