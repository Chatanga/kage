float getFogFactor(float fogDistance)
{
    float factor;

    switch (fogEquation) {
    case 0:
        factor = (fogEnd - fogDistance) / (fogEnd - fogStart);
        break;
    case 1:
        factor = exp(-fogDensity * fogDistance);
        break;
    case 2:
        factor = exp(-pow(fogDensity * fogDistance, 2.0));
        break;
    default:
        factor = 0.0;
    }
    factor = 1.0 - clamp(factor, 0.0, 1.0);

    return factor;
}

