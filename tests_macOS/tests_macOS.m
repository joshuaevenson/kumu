//
//  tests_macOS.m
//  tests_macOS
//
//  Created by Mohsen Agsen on 11/27/21.
//

#import <XCTest/XCTest.h>
#include "kutest.h"

void ktest(void);
extern int ktest_pass;
extern int ktest_fail;

@interface tests_macOS : XCTestCase

@end

@implementation tests_macOS

- (void)testMain {
  ku_test();
  XCTAssertEqual(ktest_fail, 0);
}

@end
