#!/usr/bin/env python3
import argparse
import os
import string

IMPL_TEMPLATE = string.Template('''\
package com.fivebytestudios.wildfreddy

object ${obj_name} {
  def part1(input: List[String]): Int = {
    0
  }
}
''')

TEST_TEMPLATE = string.Template('''\
package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ${obj_name}Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "${resource_dir}"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      ${obj_name}.part1(exampleData) shouldBe 0
    }

    "produce result" in {
      ${obj_name}.part1(inputData) shouldBe 0
    }
  }
}
''')

def create_file(file_path, contents):
  if os.path.exists(file_path):
    print('File {} already exists'.format(file_path))
    return
  dir_path, file_name = os.path.split(file_path)
  os.makedirs(dir_path, exist_ok=True)
  print('Creating {}'.format(file_path))
  with open(file_path, 'w') as f:
    f.write(contents)


def create_resources(base_path):
  create_file(os.path.join(base_path, 'example'), '\n')
  create_file(os.path.join(base_path, 'input'), '\n')


def create_files(base_path, day, year):
  obj_name = 'Day{:02}'.format(day)
  resource_dir = obj_name.lower()
  args = dict(obj_name=obj_name, resource_dir=resource_dir)
  create_file(os.path.join(base_path, 'main', 'scala', obj_name + '.scala'),
              IMPL_TEMPLATE.substitute(args))
  create_resources(os.path.join(base_path, 'test', 'resources', str(year), resource_dir))
  create_file(os.path.join(base_path, 'test', 'scala', obj_name + 'Test.scala'),
              TEST_TEMPLATE.substitute(args))


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('--year', type=int, default=2020, help='year')
  parser.add_argument('day', type=int, help='day number')
  args = parser.parse_args()
  create_files('src', day=args.day, year=args.year)


if __name__ == '__main__':
  main()
