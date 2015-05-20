
#include <math.h>

#include "nbody.h"

double inline_c_0_bc5bb0cf51c0f9bb3ad2aceea224bc8534787b5b(body * planets_inline_c_0, long planets_inline_c_1, long planets_inline_c_2, long planets_inline_c_3) {

        double energy = 0;
        body *planets = planets_inline_c_0;
        int i, j;

        /* Kinetic energy */
        for (i = 0; i < planets_inline_c_1; i++) {
            double vv = 0;
            int k;
            for (k = 0; k < 3; k++)
                vv += planets[i].v[k] * planets[i].v[k];

            energy += 0.5 * planets[i].mass * vv;
        }

        /* Potential energy */
        for (i = 0; i < planets_inline_c_2; i++) {
            for (j = i + 1; j < planets_inline_c_3; j++) {
                double rr = 0;
                int k;
                for (k = 0; k < 3; k++) {
                    double dx = planets[i].x[k] - planets[j].x[k];
                    rr += dx * dx;
                }
                energy -= planets[i].mass * planets[j].mass / sqrt(rr);
            }
        }

        return energy;
    
}


void inline_c_1_46bb9025e5b34fa6732607084ed7584756e9859b(body * planets_inline_c_0, int steps_inline_c_1, long planets_inline_c_2, long planets_inline_c_3, long planets_inline_c_4) {

        body *planets = planets_inline_c_0;
        const double dt = 0.01;
        int i, j, n;

        for (n = 0; n < steps_inline_c_1; n ++) {

        /* Update velocities */
        for (i = 0; i < planets_inline_c_2; i++) {
            for (j = i + 1; j < planets_inline_c_3; j++) {
                double dx[3];
                int k;
                for (k = 0; k < 3; k++)
                    dx[k] = planets[i].x[k] - planets[j].x[k];

                double rr = 0;
                for (k = 0; k < 3; k++)
                    rr += dx[k] * dx[k];

                const double mag = dt / (rr * sqrt(rr));

                double mag_ = planets[j].mass * mag;
                for (k = 0; k < 3; k++)
                    planets[i].v[k] -= mag_ * dx[k];

                mag_ = planets[i].mass * mag;
                for (k = 0; k < 3; k++)
                    planets[j].v[k] += mag_ * dx[k];
            }
        }

        /* Update positions */
        for (i = 0; i < planets_inline_c_4; i++) {
            int k;
            for (k = 0; k < 3; k++)
                planets[i].x[k] += dt * planets[i].v[k];
        }

        }
    
}

