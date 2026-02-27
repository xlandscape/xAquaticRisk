
For example, let's say you want to parameterize the response to Mullein bugs in apples. You know there are two products recommended to be used in this situation: Calypso and Admire. Here are three examples of approaches you could take (XML files shortened for clarity).

1. **Assume a farmer would use one of the two products**. For each apple field, every year xCropProtection will make a choice between the two application sequences. All attributes of the applications are independent from each other and can be changed freely. An additional application of either product can be parameterized by adding another `Application` element to one of the `ApplicationSequence` elements.
``` xml
<PPMCalendar>
    ...
    <Indications>
        <Indication type="xCropProtection.ChoiceDistribution" scales="time/year, space/base_geometry">
            <ApplicationSequence probability="0.65">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Calypso
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
            <ApplicationSequence probability="0.35">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Admire
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
        </Indication>
    </Indications>
</PPMCalendar>
```

2. **Assume a farmer would use both products**. When using this PPM Calendar, every apple field will receive one application of each product.
``` xml
<PPMCalendar>
    ...
    <Indications>
        <Indication type="xCropProtection.ChoiceDistribution" scales="time/year, space/base_geometry">
            <ApplicationSequence probability="0.1">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Calypso
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Admire
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
        </Indication>
    </Indications>
</PPMCalendar>
```

3. **Define some probabilities for a farmer using either one or both products**. If you aren't sure whether one or both products would be applied, you could also parameterize the calendar with three options.
``` xml
<PPMCalendar>
    ...
    <Indications>
        <Indication type="xCropProtection.ChoiceDistribution" scales="time/year, space/base_geometry">
            <ApplicationSequence probability="0.52">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Calypso
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
            <ApplicationSequence probability="0.28">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Admire
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
            <ApplicationSequence probability="0.2">
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Calypso
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
                <Application>
                    <Tank>
                        <Products type="list[str]" scales="other/products">
                            Admire
                        </Products>
                        ...
                    </Tank>
                    <ApplicationWindow type="xCropProtection.MonthDaySpan" scales="global">
                        05-01 to 06-08
                    </ApplicationWindow>
                    ...
                </Application>
            </ApplicationSequence>
        </Indication>
    </Indications>
</PPMCalendar>
```

Extending the previous example, you can also add `ApplicationSequence` elements representing "no treatment" or "no pest occurrence". Because all probability values must sum to 1.0, adding one of these element may be necessary depending on your needs.
