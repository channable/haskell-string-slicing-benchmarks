import pygal

def main():
    config = pygal.Config()
    config.logarithmic = True
    config.legend_at_bottom = True

    bar_chart = pygal.Bar(config)
    bar_chart.y_title = "Run times (in nanoseconds)"
    bar_chart.x_labels = ["long-0-1000", "long-5000-1000", "long-all"]
    bar_chart.add("naiveSlice", [33500, 262000, 339000])
    bar_chart.add("sliceWithRule", [834, 4940, 7.22])
    bar_chart.add("sequencedSlice", [832, 4970, 7.38])
    bar_chart.add("noinlineTakeSlice", [634, 4750, 11.3])
    bar_chart.add("reimplementedSlice", [9.59, 9.57, 9.97])
    bar_chart.render_to_file('bottlenecked-on-text-benchmarks.svg')


if __name__ == '__main__':
    main()
