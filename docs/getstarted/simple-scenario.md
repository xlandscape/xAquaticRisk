# Simple Scenario

This scenario represents the most basic use case of xCropProtection: **1 application of a product into 1 crop that occurs every year**.

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<PPMCalendar xmlns="urn:xCropProtectionLandscapeScenarioParametrization">
    <TemporalValidity scales="time/simulation"> always </TemporalValidity>
    <TargetCrops type="list[int]" scales="global"> 10 </TargetCrops>
    <Indications>
        <Indication type="xCropProtection.ChoiceDistribution" scales="time/year, space/base_geometry">
            <ApplicationSequence probability="1">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Product 1
                        </Products>
                        <ApplicationRates scales="other/products">
                            <ApplicationRate type="float" unit="g/ha" scales="global">
                                100
                            </ApplicationRate>
                        </ApplicationRates>
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 05-10
                    </ApplicationWindow>
                    <Technology scales="global">Technology</Technology>
                    <InCropBuffer type="float" unit="m" scales="global">0</InCropBuffer>
                    <InFieldMargin type="float" unit="m" scales="global">0</InFieldMargin>
                </Application>
            </ApplicationSequence>
        </Indication>
    </Indications>
</PPMCalendar>
```

## Explanation

Product 1 is applied every year for the whole duration of the simulation:

``` xml
<TemporalValidity scales="time/simulation"> always </TemporalValidity>
```

Product 1 is applied on fields with LULC type 10:

``` xml
<TargetCrops type="list[int]" scales="global"> 10 </TargetCrops>
```

The product applied has the name Product 1:

``` xml
<Products type="list[str]" scales="other/products">
    Product 1
</Products>
```

Product 1 has an application rate of 100 g/ha:

``` xml
<ApplicationRate type="float" unit="g/ha" scales="global"> 100 </ApplicationRate>
```

The application occurs between the days of May 1st and May 10th (inclusive) with each day being equally likely:

``` xml
<ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
    05-01 to 05-10
</ApplicationWindow>
```

The application uses the `Technology` with the name "Technology":

``` xml
<Technology scales="global">Technology</Technology>
```

There are no in field margins or in-crop buffers:

``` xml
<InCropBuffer type="float" unit="m" scales="global">0</InCropBuffer>
<InFieldMargin type="float" unit="m" scales="global">0</InFieldMargin>
```

## Illustration

<img src="../img/simple-scenario.PNG" alt="xCP parameterisation entities and their relationship" width="700"/>

See also ['Indications'](../reference/glossary.md#indication) for the full picture of xCP entities and their relationship.
